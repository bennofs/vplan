{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
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
{-# LANGUAGE ConstraintKinds            #-}

-- | The schedule data type.
module Data.VPlan.Schedule (
    Schedule(..)
  , schedule
  , Supported(new)
  , ScheduleType
  , ModSame
  , ModInstance
  ) where

import           Control.Lens
import           GHC.Exts
import           Data.Data
import qualified Data.VPlan.At        as A
import           Data.VPlan.TH
import           GHC.Generics
import           Data.Foldable (Foldable(..))
import           Data.Aeson


-- | The type of a schedule. This is like fix, but with the indices added.
newtype Schedule s i v = Schedule (s (Schedule s) i v) deriving (Generic)
makeIso ''Schedule
genIxedInstances ''Schedule
makeTypeable ''Schedule

deriving instance (ModInstance Show s i v) => Show (Schedule s i v)
deriving instance (ModInstance Data s i v, Typeable (Schedule s i v)) => Data (Schedule s i v)
deriving instance (ModInstance Eq s i v) => Eq (Schedule s i v)
deriving instance (ModInstance (A.Contains f) s i v) => A.Contains f (Schedule s i v)
deriving instance (ModInstance (A.Ixed f) s i v, ModSame Index s i v, ModSame IxValue s i v) => A.Ixed f (Schedule s i v)
deriving instance (ModInstance1 Functor s i) => Functor (Schedule s i)
deriving instance (ModInstance1 Contravariant s i) => Contravariant (Schedule s i)
deriving instance (ModInstance2 Profunctor s) => Profunctor (Schedule s)
deriving instance (ModInstance2 Bifunctor s) => Bifunctor (Schedule s)
deriving instance (ModInstance1 Foldable s i) => Foldable (Schedule s i)
deriving instance (ModInstance1 Traversable s i) => Traversable (Schedule s i)
deriving instance (ModInstance FromJSON s i v) => FromJSON (Schedule s i v)
deriving instance (ModInstance ToJSON s i v) => ToJSON (Schedule s i v)

instance (Data (Schedule s i v)) => Plated (Schedule s i v)

type instance Index (Schedule s i v) = i
type instance IxValue (Schedule s i v) = v
type instance Modifiers (Schedule s i v) = s

type family ScheduleType m :: *
type instance ScheduleType a = Schedule (Modifiers a) (Index a) (IxValue a)

type ModSame t s i v = (t (Schedule s i v) ~ t (s (Schedule s) i v))
type ModInstance (x :: * -> Constraint) s i v = x (s (Schedule s) i v)
type ModInstance1 (x :: (* -> *) -> Constraint) s i = x (s (Schedule s) i)
type ModInstance2 (x :: (* -> * -> *) -> Constraint) s = x (s (Schedule s))

-- | Assert that a Schedule s supports a given modifier m.
class Supported m s where

  -- | Construct a Schedule with that modifier.
  new :: m s i v -> s i v
