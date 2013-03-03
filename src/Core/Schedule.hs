{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TupleSections, FlexibleInstances, OverlappingInstances, UndecidableInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
-- | Contains the schedule data type and combinators for building schedules.
module Core.Schedule (
  -- * The schedule
  -- 
  -- 
  )
       where 

import Core.Course
import qualified Data.DList as DL
import Data.DList (DList)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import Control.Arrow
import Control.Lens
import qualified Data.Map as M



{-| A base schedule, on which more complex schedules can be build. 
    The base schedule is a map from the day of the week and the lesson number to the course 
    that shall be taking place at that time.
 -}
type BaseSchedule = M.Map Day (M.Map Int Course)

{-| A course locator is used to point to a specific course in a schedule.
 -}
data CourseLocator = CourseLocator { 
  _courseDay :: Date, -- ^ The day on which the curse to be located is taking place. This is relative to the schedule's origin.
  _courseNr :: Int    -- ^ The number of the course to be located.
}
makeLenses ''CourseLocator

{-| A change to the 'BaseSchedule'. The current day for the change to be applied is always 0. You can offset the day by using
    the 'Offset' data constructor.
 -}
data Change = Move CourseLocator CourseLocator   -- ^ Move c d moves 'Course' from d to c overwriting any existing 'Course' at c
            | Set CourseLocator (Maybe Course)   -- ^ Set l c sets the 'Course' at l to c, overwriting any existing 'Course'. If c is Nothing, it 
                                                 --   cancels the curse at l.
            | RepeatEvery Weeks Change           -- ^ RepeatEvery w c repeats the 'Change' c every w weeks
            | Offset DateTimespan Change         -- ^ Offset t c offsets the 'Change' c for t.
type Changes = [Change] 

{-| The data type for a schedule. A schedule is just the base schedule plus
    a list of ordered changes.
 -}
data Schedule = Schedule {
  _baseSchedule :: BaseSchedule,  -- ^ The base schedule
  _changes :: Changes             -- ^ List of changes to the base schedule
  }
makeLenses ''Schedule

{-| Set a map of courses to use for a given day. Use this combinator to build
    the base schedule.

    Example: on Monday x uses the schedule x for monday. 
 -}
on :: Day -> M.Map Int Course -> Writer (Endo BaseSchedule) ()
on d w = tell $ Endo $ M.insert d w

{-| Builds a map of courses from the number of the first course and a list of courses

    Example:
    >>> dayCourses 2 [a, b, c]
    M.fromList [(2,a), (3,b), (4,c)]
 -}
dayCourses :: Int -> [Course] -> M.Map Int Course
dayCourses s cs = M.fromList $ zip [s..] cs

{-| Runs the base schedule builder and returns the base schedule.
 -}
generateBaseSchedule :: Writer (Endo BaseSchedule) () -> BaseSchedule
generateBaseSchedule s = appEndo (execWriter s) M.empty

{-| Add some changes to a schedule. The changes are placed after the current changes in the schedule.
 -}
addChanges :: Schedule -> Changes -> Schedule
addChanges = flip $ (changes %~) . (++)

{-| A mini-DSL for lists of things.
 -}
type Builder b = Writer (DList b) ()

{-| Run a builder. 
 -}
runBuilder :: Builder b -> [b]
runBuilder b = DL.toList $ (execWriter b)

{-| Offset all changes in a change builder by the given amount of time.
 -}
offset :: DateTimespan -> Builder Change -> Builder Change
offset t = censor (DL.map (Offset t))

{-| Repeat all changes in a change builder every n weeks.
 -}
repeatEvery :: Weeks -> Builder Change -> Builder Change
repeatEvery n = censor (DL.map (RepeatEvery n))

{-| Lookup a day in the base schedule.
 -}
baseLookup :: BaseSchedule -> Date -> M.Map Int [Course]
baseLookup s d = maybe M.empty (M.map (:[])) $ M.lookup (d ^. dateDay) s

{-| Lookup a day in a changed schedule with a list of changes
 -}
changesLookup :: (Date -> M.Map Int [Course]) -> Changes -> Date -> M.Map Int [Course]
changesLookup u (c:cs) d = changesLookup (changeLookup u c) cs d
changesLookup u _ d = u d

{-| Lookup a day in a changed schedule.
 -}
changeLookup :: (Date -> M.Map Int [Course]) -> Change -> Date -> M.Map Int [Course]
changeLookup u (Move t f) d = changesLookup u [Set t ch, Set f Nothing] d
  where ch = u (f ^. courseDay) ^? ix (f ^. courseNr) . _last
changeLookup u (Set t c) d 
  | d == t ^. courseDay = u d & at (t ^. courseNr) %~ liftM2 (:) c 
changeLookup u (RepeatEvery (Weeks w) c) d
  | views (dateWeek . from week) (`mod` w) d == 0 = changeLookup u c $ d & dateWeek . from week .~ 0
changeLookup u (Offset t c) d
  | views daysFromOrigin (>= toDays t) d = changeLookup u c $ d & daysFromOrigin -~ (toDays t)
changeLookup fallback _ d = fallback d   

{-| Runs a schedule. Determines the list of courses for a given day.
 -}
runSchedule :: Schedule                 -- ^ The schedule
            -> Date                     -- ^ The date to lookup in the schedule
            -> [(Date, Date)]           -- ^ List of ranges of dates to exclude in the calculation (for example holidays)
            -> M.Map Int Course        -- ^ The courses taking place at the given day.
runSchedule s d es 
  | any ((&&) <$> (d>=) . fst <*> (d<=) . snd) es = M.empty
  | otherwise = M.fromList $ courses ^@.. traversed <. _last
  where dateRangeSize (r1,r2) = r2 ^. daysFromOrigin - r1 ^. daysFromOrigin
        daysSkip = sum $ [dateRangeSize r | r <- es, fst r < d] 
        d' = backward daysSkip d
        courses = changesLookup (baseLookup $ s ^. baseSchedule) (s ^. changes) d'
        