{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | The schedule data type.
module Data.VPlan.Schedule (
    Schedule(..)
  , schedule
  , Supported(new)
  , ScheduleType
  ) where

import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At as A
import           Data.VPlan.TH

-- | The type of a schedule. This type is just 'fix' at the type level.
newtype Schedule i v s = Schedule (s (Schedule i v s))
makeIso ''Schedule
genTypeable ''Schedule
genIxedInstances ''Schedule

deriving instance (Data (s (Schedule i v s)), Typeable (Schedule i v s)) => Data (Schedule i v s)

instance (Data (s (Schedule i v s)), Typeable1 s, Typeable v, Typeable i) => Plated (Schedule i v s)
type instance Index (Schedule i v s) = i
type instance IxValue (Schedule i v s) = v

type instance Modifiers (Schedule i v s) = s

-- | Get the schedule type corresponding to a given modifier type.
type family ScheduleType m :: *
type instance ScheduleType a = Schedule (Index a) (IxValue a) (Modifiers a)

deriving instance (A.Contains f (s (Schedule i v s))) => A.Contains f (Schedule i v s)
deriving instance (A.Ixed f (s (Schedule i v s)), IxValue (s (Schedule i v s)) ~ v,
                   Index (s (Schedule i v s)) ~ i) => A.Ixed f (Schedule i v s)
deriving instance (Eq (s (Schedule i v s))) => Eq (Schedule i v s)

-- | Assert that a Schedule s supports a given modifier m.
class Supported m s where

  -- | Construct a Schedule with that modifier.
  new :: m s -> s
