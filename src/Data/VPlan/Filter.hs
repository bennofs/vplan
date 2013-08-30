{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
-- | Operations for filtering schedules
module Data.VPlan.Filter
  ( cleanTop
  , cleanSchedule
  ) where

import           Control.Lens
import           Data.Data
import           Data.Data.Lens
import           Data.Functor
import           Data.Monoid
import           Data.Proxy
import           Data.VPlan.Modifier.Empty
import           Data.VPlan.Schedule

-- | Test whether a data structure holds a value of the given type, recursively.
containsType :: forall a. forall s. (Data s, Typeable a) => Proxy a -> s -> Bool
containsType _ = has (template :: Getting Any s a)

-- This type is not exported, so it's not visible from other modules. That means that other modules can't cheat in instances by makeing special ones for this type.
data Unique = Unique deriving (Typeable, Data)

-- | Replace the toplevel modifier with Empty when it doesn't contain a value.
cleanTop :: forall i. forall s. forall v. (Supported Empty (Schedule s), Data (Schedule s i Unique), Functor (Schedule s i)) => Schedule s i v -> Schedule s i v
cleanTop s
  | containsType (Proxy :: Proxy Unique) $ Unique <$ s = s
  | otherwise = new Empty

-- | Clean a whole Schedule
cleanSchedule :: (Supported Empty (Schedule s), Data (Schedule s i Unique), Data (Schedule s i v), Functor (Schedule s i)) => Schedule s i v => Schedule s i v
cleanSchedule = transform cleanTop
