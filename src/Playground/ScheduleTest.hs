import Core.Course  
import Core.Schedule
import Core.SimpleSchedule
import Data.List
import qualified Core.FunctionMap as M
import Control.Applicative
import Control.Lens

-- First define some courses
mathC = Course (Subject "Maths" "Ma") (Teacher "X" "X") (Room 213)
germanC = Course (Subject "German" "Ge") (Teacher "Y" "Y") (Room 212)
englishC = Course (Subject "English" "En") (Teacher "Z" "Z") (Room 110)

-- Then define the base schedule
sched = do
   on Monday $ dayCourses 1 [mathC, germanC, englishC, englishC, germanC]
   on Tuesday $ dayCourses 2 [germanC, germanC, englishC] -- On tuesday, courses start at lesson number 2
   on Wednesday $ dayCourses 1 [germanC, englishC, germanC, mathC]
   on Thursday $ dayCourses 1 [englishC, englishC, mathC, germanC]
   on Friday $ dayCourses 1 [mathC, englishC, germanC, germanC]
   offsetModifier (1 ^. weeks) $ repeatEvery (2 ^. weeks) $ cancelCourse $ reference (date 0 Monday) 1

sched' = do
  on Monday $ dayCourses 1 [mathC]
  
-- Get the numbers of all possible courses
getCourseIds = sort . nub . map getCourseNumber . toListOf (modifiers . traverse . traverse)
  where getCourseNumber (Set x _) = x ^. refId
        getCourseNumber (Repeat _ c) = getCourseNumber c
        getCourseNumber (Offset _ c) = getCourseNumber c
        
-- Render a timetable to a list of columns, where each column is a list of strings
renderSchedule s = ("    ":map (pad 4 . show) courseIds) : renderScheduleFromTo courseIds (date 0 Monday) (date (review weeks per) Monday) s
  where courseIds = getCourseIds s
        (Just per) = schedulePeriod s

pad :: Int -> String -> String
pad x s 
  | length s < x = s ++ replicate (x - length s) ' '
  | otherwise = s

runSchedule = runExpandedSchedule . expandSchedule

-- Render a schedule from a given time to a given time
renderScheduleFromTo :: [Int] -> Date -> Date -> Schedule SimpleModifier -> [[String]]
renderScheduleFromTo cnrs f t s = map renderDay [f .. (backward (1 ^. days) t)]
  where renderDay x = x ^. dateDay . to (pad 4 . take 2 . show) : (map (maybe "--  " renderCourse) $ map (\y -> runs ^? ix x . ix y) cnrs)
        renderCourse (Course (Subject _ short) _ _) = pad 4 short
        runs = runSchedule s
main = do
  traverse (putStrLn . concat) $ transpose $ renderSchedule $ runScheduleBuilder $ sched
  print $ runSchedule (runScheduleBuilder sched) ^? ix (date 10000 Monday) . ix 1