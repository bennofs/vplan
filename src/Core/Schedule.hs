{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TupleSections, FlexibleInstances, OverlappingInstances, UndecidableInstances, MultiParamTypeClasses, TypeSynonymInstances, NoImplicitPrelude, TypeFamilies, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | This module provides the schedule data type and combinators for building schedules.
module Core.Schedule (
  
  {-|
    A schedule is just a list of modifications. This module provides the basic interface for modifications and a  
    schedule type. For a concrete implementation, look at 'Core.SimpleSchedule'.
  -}
  
    ExpandedSchedule, runExpandedSchedule
  , Schedule, modifiers, schedulePeriod, expandSchedule, modifySchedule, (|&)
  , ScheduleModifier, PeriodType, expandModifier, modifierPeriod
  ,  Builder, ScheduleBuilder, runBuilder, mapBuilder, buildItem, buildParallel, runScheduleBuilder, mapScheduleBuilder 
  , module Core.Time, module Control.Monad.Writer
  )
       where 

import Core.Course
import Core.Time
import qualified Core.FunctionMap as M
import qualified Data.DList as DL
import Data.DList (DList)
import NumericPrelude hiding ((^?), foldr, foldl, concatMap, foldr1, foldl1, sequence_)
import Control.Monad.Reader hiding (sequence_)
import Control.Monad.Writer hiding (sequence_)
import Control.Lens
import Data.Foldable

-- | A fully expanded schedule. This is the result of evaluating a schedule. 
-- There can be several possible courses at one point in the schedule (Courses that are overwritten are never 
-- removed from the schedule, but instead, a new course is added at the same time), this is why it's a
-- a map from (Date,Int) to a list of 'Course's.
-- You should not use this data type to save a schedule to a file, because it is
-- infinite if the original schedule uses RepeatEvery at least once.
type ExpandedSchedule = M.Map Date (M.Map Int [Maybe Course])

-- | Convert an 'ExpandedSchedule' to a map, where only the most recent courses are 
-- kept.
runExpandedSchedule :: ExpandedSchedule -> M.Map Date (M.Map Int Course)
runExpandedSchedule = M.map cat
  where cat x = M.fromAscList $ x ^@.. itraversed <. (_head . traversed)

-- | The repeat interval type for modifierPeriod.
type family PeriodType a

-- | A class for schedule modifiers. Modifiers have access to the current state of the schedule.
class ScheduleModifier a where
  
  -- | Expand the modifier.
  expandModifier :: a                  -- ^ The modifier
                 -> ExpandedSchedule   -- ^ The current schedule
                 -> ExpandedSchedule   -- ^ The additions to the current schedule
                 
  -- | Get the period of a modifier, i.e. how long it takes till the modifier repeats. Not all 
  -- modifier have a repeat interval, so the result value is a maybe. You only need to overwrite this
  -- if you have periodic modifiers.
  modifierPeriod :: a -> Maybe (PeriodType a)
  modifierPeriod _ = Nothing

-- | The data type for a schedule. A schedule is just a list of ordered modifications to an empty schedule. A modification is itself
-- a list of modifiers, which will be all applied to the same schedule and don't affect each other. The result is then combined and
-- used for the input to the next modification.
newtype Schedule c = Schedule {
  _modifiers :: [[c]] -- ^ List of modifications to the base schedule
  } deriving (Monoid, Show)
makeLenses ''Schedule

-- | Apply a 'ScheduleModifier' to an 'ExpandedSchedule'.
(|&) :: (ScheduleModifier m) => ExpandedSchedule -> m -> ExpandedSchedule
s |& m = s <> expandModifier m s

-- | Add some modifiers to a 'Schedule'. The modifications are placed after the current modifiers in the 'Schedule'.
modifySchedule :: Schedule c -> [[c]] -> Schedule c
modifySchedule = flip $ (modifiers %~) . (++)

-- | Expand a schedule, applying the modifications one after another, from head to tail of the list.
-- The resulting ExpandedSchedule might be infinite.
expandSchedule :: (ScheduleModifier m) => Schedule m -> ExpandedSchedule
expandSchedule = foldr expand mempty . view modifiers
  where expand x c = c <> mconcat (map (`expandModifier` c) x)
        
-- | Get the period of a schedule. Not all schedules have a periodic repeat interval, so the result type is a maybe.
schedulePeriod :: (ScheduleModifier m, HasDaysPrism (PeriodType m)) => Schedule m -> Maybe (PeriodType m)
schedulePeriod s = (^? daysPrism) . uncurry (foldr lcs) =<< periods ^? _Cons
  where periods = map (view toDays) $ s ^.. modifiers . traverse . traverse . to modifierPeriod . folded

-- | A mini-DSL for lists of things
type Builder b = Writer (DList b) ()

-- | Run a builder
runBuilder :: Builder b -> [b]
runBuilder b = DL.toList $ execWriter b

-- | Add a single item to a 'Builder'
buildItem :: a -> Builder a
buildItem = tell . DL.singleton

-- | Run some 'ScheduleBuilder's in parallel, meaning they won't influence each other and the result will be combined. 
buildParallel :: ScheduleBuilder m -> ScheduleBuilder m
buildParallel bs = listen bs >>= buildItem . sequence_ . DL.toList . snd

-- | Map a function over the output of a builder
mapBuilder :: (a -> a) -> Builder a -> Builder a
mapBuilder f = censor (DL.map f)

-- | A handy alias
type ScheduleBuilder a = Builder (Builder a)

-- | Map a function over a schedule builder
mapScheduleBuilder :: (a -> a) -> ScheduleBuilder a -> ScheduleBuilder a
mapScheduleBuilder f = mapBuilder (mapBuilder f)

-- | Runs a builder and creates a schedule out of the returned list of modifiers
runScheduleBuilder :: (ScheduleModifier m) => ScheduleBuilder m -> Schedule m
runScheduleBuilder = Schedule . map runBuilder . runBuilder