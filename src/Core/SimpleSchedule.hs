{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : $Header$
-- Description : A simple schedule based on the modifiers Combine, Limit, Copy and Constant.
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
  , delete
  , create
  , at
  , buildCombine
  ) where

import           Control.Lens           hiding (at)
import           Core.Builder
import           Core.Modifier.Combine
import           Core.Modifier.Constant
import           Core.Modifier.Copy
import qualified Core.Modifier.Empty    as E
import           Core.Modifier.Enum
import           Core.Modifier.Limit
import           Core.Schedule

-- | The modifiers of the simple schedule
type SimpleModifiers = Limit ||< Combine >||< Copy >||< E.Empty >|| Constant

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
delete :: SimpleSchedule i v
delete = enumSchedule E.empty

-- | Set a value.
create :: v -> SimpleSchedule i v
create = enumSchedule . view constant

-- | Apply a modifier only at a given item in the schedule.
at :: i -> SimpleSchedule i v -> SimpleSchedule i v
at w a = equal w !<| a

-- | Build a list of schedules to combine with (-||-).
buildCombine :: Builder (SimpleSchedule i v) () -> SimpleSchedule i v
buildCombine = foldr (-||-) delete . runBuilder
