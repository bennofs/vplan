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
import           Data.VPlan.Class


-- | The type of a schedule. This is like fix, but with the indices added.
newtype Schedule s c i v = Schedule (s (Schedule s) c i v) deriving (Generic)
makeIso ''Schedule
genIxedInstances ''Schedule
makeTypeable ''Schedule

deriving instance ModInstance Show s c i v => Show (Schedule s c i v)
deriving instance ModInstance Read s c i v => Read (Schedule s c i v)
deriving instance (ModInstance Data s c i v, Typeable (Schedule s c i v)) => Data (Schedule s c i v)
deriving instance ModInstance Eq s c i v => Eq (Schedule s c i v)
deriving instance ModInstance (A.Contains f) s c i v => A.Contains f (Schedule s c i v)
deriving instance (ModInstance (A.Ixed f) s c i v, ModSame Index s c i v, ModSame IxValue s c i v) => A.Ixed f (Schedule s c i v)
deriving instance ModInstance1 Functor s c i => Functor (Schedule s c i)
deriving instance ModInstance1 Contravariant s c i => Contravariant (Schedule s c i)
deriving instance ModInstance2 Profunctor s c => Profunctor (Schedule s c)
deriving instance ModInstance2 Bifunctor s c => Bifunctor (Schedule s c)
deriving instance ModInstance1 Foldable s c i => Foldable (Schedule s c i)
deriving instance ModInstance1 Traversable s c i => Traversable (Schedule s c i)
deriving instance ModInstance FromJSON s c i v => FromJSON (Schedule s c i v)
deriving instance ModInstance ToJSON s c i v => ToJSON (Schedule s c i v)

instance Data (Schedule s c i v) => Plated (Schedule s c i v)

type instance Index (Schedule s c i v) = i
type instance IxValue (Schedule s c i v) = v

type ModSame t s c i v = (t (Schedule s c i v) ~ t (s (Schedule s) c i v))
type ModInstance (x :: * -> Constraint) s c i v = x (s (Schedule s) c i v)
type ModInstance1 (x :: (* -> *) -> Constraint) s c i = x (s (Schedule s) c i)
type ModInstance2 (x :: (* -> * -> *) -> Constraint) s c = x (s (Schedule s) c)

-- | Assert that a Schedule s supports a given modifier m.
class Supported m s where

  -- | Construct a Schedule with that modifier.
  new :: m s c i v -> s c i v

instance (ModInstance Periodic s c i v, ModSame Index s c i v) => Periodic (Schedule s c i v) where
  interval = interval . review schedule

instance (ModInstance Limited s c i v, ModSame Index s c i v) => Limited (Schedule s c i v) where
  imax = imax . review schedule
  imin = imin . review schedule
