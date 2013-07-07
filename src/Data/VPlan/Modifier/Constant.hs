{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | A modifier that always returns a constant value.
module Data.VPlan.Modifier.Constant (
    Constant(..)
  , constant  ) where

import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At    as A
import           Data.VPlan.Class
import           Data.VPlan.TH

-- | A modifier that always returns the same value, for all possible indices.
newtype Constant s = Constant (IxValue s)
makeModifier ''Constant
makeIso ''Constant
deriveClass ''Constant

deriving instance (Data (IxValue s), Data s) => Data (Constant s)
deriving instance (Eq (IxValue s)) => Eq (Constant s)

instance (Gettable f) => A.Contains f (Constant s) where contains = containsTest  $ const $ const True
instance (Functor f) => A.Ixed f (Constant s)      where ix i f   = from constant $ indexed f i
