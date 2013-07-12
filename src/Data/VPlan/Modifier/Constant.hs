{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
newtype Constant (s :: * -> * -> *) i v = Constant v deriving (Eq)
makeModifier ''Constant
makeIso ''Constant
deriveClass ''Constant

deriving instance (Typeable2 s, Typeable i, Typeable v, Data v) => Data (Constant s i v)

instance (Gettable f) => A.Contains f (Constant s i v)                 where contains = containsTest  $ const $ const True
instance (Functor f, v ~ IxValue (s i v)) => A.Ixed f (Constant s i v) where ix i f   = from constant $ indexed f i
instance Functor (Constant s i) where  fmap f    (Constant x) = Constant $ f x
instance Bifunctor (Constant s) where  bimap _ f (Constant x) = Constant $ f x
instance Profunctor (Constant s) where dimap _ f (Constant x) = Constant $ f x
