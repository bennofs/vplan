module Core.Schedule2 where

import Core.Timespan
import Core.Course

newtype LessonNr = LessonNr Int
data Date d = Date d | After (Timespan d)

data Modifier d  = RepeatEvery (Timespan d) | Start (Date d) | End (Date d) | Except (Date d)
data Action d a = Action [Modifier d] a
type Schedule = [Action (Date Int) [Course]]

