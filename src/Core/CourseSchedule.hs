{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : $Header$
-- Description : Provides a collection of functions for modifiers to build schedules for courses.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable
module Core.CourseSchedule where

import           Control.Lens
import           Core.Builder
import           Core.Modifier.Combining
import           Core.Modifier.Constant
import           Core.Modifier.Copy
import           Core.Modifier.Enum
import           Core.Modifier.Parallel
import           Core.Modifier.Sequential
import Core.Modifier.Limit
import           Core.Schedule
import           Core.Time
import Data.Monoid
import qualified Data.Sequence            as S

-- | The type of a reference to a course in the schedule.
data CourseReference = CourseReference {_courseReferenceDate :: Date, _courseReferenceNumber :: Int} deriving (Eq, Ord)

-- | Smart constructor for CourseReference
reference :: Date -> Int -> CourseReference
reference = CourseReference

-- | The modifiers of the course schedule
type CourseModifiers m = Parallel ||< Limit CourseReference >||< Sequential >||< Combining >||< Constant m >|| Copy CourseReference

-- | Our definition of a schedule.
type CourseSchedule m = Schedule (CourseModifiers m)

-- | Combine two course schedules sequentially.
(->>-) :: CourseSchedule m -> CourseSchedule m -> CourseSchedule m
s ->>- t = sequentialSchedule $ item s >> item t

-- | Combine two course schedules sequentially (reversed).
(-<<-) :: CourseSchedule m -> CourseSchedule m -> CourseSchedule m
(-<<-) = flip (->>-)

-- | Combine two course schedule into a parallel schedule.
(-||-) :: CourseSchedule m -> CourseSchedule m -> CourseSchedule m
s -||- t = parallelSchedule $ item s >> item t

-- | Combine two schedules sequentially, but wrap the schedules in a Combining modifier.
(-<>-) :: CourseSchedule m -> CourseSchedule m -> CourseSchedule m
s -<>- t = sequentialSchedule $ do
  scheduleItem $ s ^. combining
  scheduleItem $ t ^. combining

-- | Convert a builder of schedules into a sequential 'Schedule'.
sequentialSchedule :: Builder (CourseSchedule m) () -> CourseSchedule m
sequentialSchedule = view schedule . enumValue . view sequential . S.fromList . runBuilder

-- | Convert a builder of schedules into a parallel 'Schedule'.
parallelSchedule :: Builder (CourseSchedule m) () -> CourseSchedule m
parallelSchedule = view schedule . enumValue . view parallel . S.fromList . runBuilder

-- | Set a course at a given course reference.
setCourse :: (Monoid m) => CourseReference -> m -> CourseSchedule m
setCourse r c = enumSchedule (equal r) -<<- enumSchedule (c ^. constant)
