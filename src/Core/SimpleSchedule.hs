{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
-- | A simple implementation of the Schedule interface.
module Core.SimpleSchedule( 
    CourseReference(), refDate, refId, reference, resolveReference
  , SimpleModifier(..), swapCourse, moveCourse, setCourse, cancelCourse, repeatEvery, offsetModifier
  , on, dayCourses                                                         
  )
       where

import Core.Schedule
import Core.Course
import qualified Core.FunctionMap as M
import Control.Lens

-- | A reference to a course in the schedule.
data CourseReference = CourseReference {
  _refDate :: Date,           -- ^ The date at which the referenced course takes place.
  _refId :: Int               -- ^ The id of the course at the given date to be referenced.
  } deriving (Show)
makeLenses ''CourseReference

-- | Make a reference to a course
reference :: Date -> Int -> CourseReference
reference = CourseReference

-- | Resolves a course reference and returns Just c if the given course exists, else returns Nothing.
resolveReference :: CourseReference -> ExpandedSchedule -> Maybe Course
resolveReference r s = join $ s ^? ix (r ^. refDate) . ix (r ^. refId) . _last


-- | A basic modifier that provides already a lot of flexibility.
data SimpleModifier
  -- | Set r w sets the course at the location r to either a course or to the course from a given location.
  = Set CourseReference (Either CourseReference (Maybe Course)) 
  -- | Repeat n s repeats the modifier s every n weeks, starting from date 0 Monday again.
  | Repeat Weeks SimpleModifier
  -- | Offset o s offsets the modifier s by the given amount o.
  | Offset DateTimespan SimpleModifier
  deriving (Show)
scheduleAtReference :: CourseReference -> Maybe Course ->  ExpandedSchedule
scheduleAtReference l c = mempty & at (l ^. refDate) ?~ (mempty & at (l ^. refId) ?~ [c])

type instance PeriodType SimpleModifier = Weeks

instance ScheduleModifier SimpleModifier where
  
  expandModifier (Set l (Right c)) _ = scheduleAtReference l c
  expandModifier (Set l (Left r)) s = scheduleAtReference l $ resolveReference r s
  expandModifier (Repeat w m) s = M.fromAscList . rep . M.toList $ expandModifier m s
    where rep = concat . iterate next
          next = map (_1 %~ forward w)
          
  expandModifier (Offset t m) s = M.fromAscList $ expandModifier m s ^@.. reindexed (forward t) itraversed
  modifierPeriod (Offset _ m) = modifierPeriod m
  modifierPeriod (Repeat w _) = Just w
  modifierPeriod _ = Nothing

-- | Swap two courses, using moves.
swapCourse :: CourseReference -> CourseReference -> ScheduleBuilder SimpleModifier
swapCourse x y = buildParallel $ moveCourse x y >> moveCourse y x

-- | Move one course to another location. Uses the earliest course that was set at the given
-- location, not the most recent one.
moveCourse :: CourseReference                 -- ^ Course to move from
           -> CourseReference                 -- ^ Course to move to
           -> ScheduleBuilder SimpleModifier  -- ^ A builder that builds the move.
moveCourse t f = buildParallel $ referenceCourse t f >> cancelCourse f

-- | Set the course at a given location.
setCourse :: CourseReference -> Course -> ScheduleBuilder SimpleModifier
setCourse x = buildItem . buildItem . Set x . Right . Just

-- | Reference a course at a given location.
referenceCourse :: CourseReference -> CourseReference -> ScheduleBuilder SimpleModifier
referenceCourse x = buildItem . buildItem . Set x . Left

-- | Cancel the course at the given location.
cancelCourse :: CourseReference -> ScheduleBuilder SimpleModifier
cancelCourse = buildItem . buildItem . flip Set (Right Nothing)

-- | Repeat something every n weeks
repeatEvery :: Weeks -> ScheduleBuilder SimpleModifier -> ScheduleBuilder SimpleModifier
repeatEvery w = mapScheduleBuilder (Repeat w)

-- | Offset a 'ScheduleBuilder' of 'SimpleModifier's by the given amount of time.
offsetModifier :: (HasDaysPrism t) => t -> ScheduleBuilder SimpleModifier -> ScheduleBuilder SimpleModifier
offsetModifier t = mapScheduleBuilder (Offset $ t ^. toDays . from daysIso)

-- | Set a map of courses to use for a given day. Use this combinator to build the base schedule.
-- Example: on Monday x uses the schedule x for monday. 
on :: Day -> M.Map Int Course -> ScheduleBuilder SimpleModifier
on d = itraverse_ s
  where s i v = repeatEvery (1 ^. weeks) $ setCourse (reference dat i) v
        dat = date 0 d

-- | Builds a map of courses from the number of the first course and a list of courses
-- Example:
-- >>> dayCourses 2 [a, b, c]
-- M.fromList [(2,a), (3,b), (4,c)]
dayCourses :: Int -> [Course] -> M.Map Int Course
dayCourses s cs = M.fromAscList $ zip [s..] cs