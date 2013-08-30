{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | A modifier that always returns a constant value.
module Data.VPlan.Modifier.Constant (
    Constant(..)
  , constant
  ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Data.Aeson.Types
import           Data.Data
import           Data.Foldable       (Foldable (..))
import qualified Data.VPlan.At       as A
import           Data.VPlan.Class
import           Data.VPlan.TH
import           GHC.Generics        hiding (from)

-- | A modifier that always returns the same value, for all possible indices.
newtype Constant (s :: * -> * -> *) i v = Constant v deriving (Eq, Generic)
makeModifier ''Constant
makeIso ''Constant
deriveClass ''Constant

deriving instance Show v => Show (Constant s i v)
deriving instance Read v => Read (Constant s i v)
deriving instance (Typeable2 s, Typeable i, Typeable v, Data v) => Data (Constant s i v)

instance (Gettable f) => A.Contains f (Constant s i v)                 where contains = containsTest  $ const $ const True
instance (Functor f, v ~ IxValue (s i v)) => A.Ixed f (Constant s i v) where ix i f   = from constant $ indexed f i
instance Functor (Constant s i) where  fmap f    (Constant x) = Constant $ f x
instance Bifunctor (Constant s) where  bimap _ f (Constant x) = Constant $ f x
instance Profunctor (Constant s) where dimap _ f (Constant x) = Constant $ f x
instance Foldable (Constant s i) where fold f (Constant v) = f v
instance Traversable (Constant s i) where traverse f (Constant v) = Constant <$> f v

instance (FromJSON v) => FromJSON (Constant s i v) where
  parseJSON (Object o) = Constant <$> o .: "value"
  parseJSON v = typeMismatch "Object" v

instance (ToJSON v) => ToJSON (Constant s i v) where
  toJSON (Constant v) = object ["value" .= v]
