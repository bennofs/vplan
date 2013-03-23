{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : $Header$
-- Description : A simple schedule based on the modifiers Combine, Limit, Reference, Enum, Empty
--               and Constant.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable
module Core.SimpleSchedule (
    SimpleModifiers()
  , SimpleSchedule()
  , (!<-)
  , (!<|)
  , (-||-)
  , empty
  , single
  , at
  , reference
  , move
  , swap
  , buildCombine
  ) where

import           Control.Lens           hiding (at)
import           Core.Builder
import           Core.Modifier.Combine
import           Core.Modifier.Constant
import qualified Core.Modifier.Reference as R
import qualified Core.Modifier.Empty    as E
import           Core.Modifier.Enum
import           Core.Modifier.Limit
import           Core.Schedule

-- | The modifiers of the simple schedule
type SimpleModifiers = Limit ||< Combine >||< R.Reference >||< E.Empty >|| Constant

-- | Our definition of a schedule.
type SimpleSchedule i v = Schedule i v SimpleModifiers

-- | Apply a modifier to another modifier
(!<-) :: (MakeTypeEnum a (s (Schedule i v s)), MakeTypeEnum b (t (Schedule i v t)))
         => (Schedule i v t -> a) -> b -> Schedule i v s
a !<- b = enumSchedule $ a $ enumSchedule b

-- | Apply a modifier to a schedule.
(!<|) :: MakeTypeEnum a (s (Schedule i v s)) => (t -> a) -> t -> Schedule i v s
a !<| b = enumSchedule $ a b

-- | Combine two simple schedules using the 'Combine' modifier.
(-||-) :: SimpleSchedule i v -> SimpleSchedule i v -> SimpleSchedule i v
s -||- t = enumSchedule $ combine s t

-- | This is just an empty schedule.
empty :: SimpleSchedule i v
empty = enumSchedule E.Empty

-- | A schedule that contains a single value.
single :: v -> SimpleSchedule i v
single = enumSchedule . view constant

-- | A schedule that contains the same value as the given schedule at some index.
reference :: i -> SimpleSchedule i v -> SimpleSchedule i v
reference x = enumSchedule . R.reference x

-- | Apply a modifier only at a given item in the schedule.
at :: i -> SimpleSchedule i v -> SimpleSchedule i v
at w a = equal w !<| a

-- | Build a list of schedules to combine with (-||-).
buildCombine :: Builder (SimpleSchedule i v) () -> SimpleSchedule i v
buildCombine = foldr (-||-) empty . runBuilder

-- | Move an item to another place. @move source target@ moves an item at index source to
-- index target.
move :: i -> i -> SimpleSchedule i v -> SimpleSchedule i v
move f t s = at f empty -||- at t (reference f s)

-- | Swap two items at given indices.
swap :: i -> i -> SimpleSchedule i v -> SimpleSchedule i v
swap a b s = at b (reference a s) -||- at a (reference b s)
