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

-- | A modifier that contains no value.
module Data.VPlan.Modifier.Empty (
    Empty(..)
  ) where

import           Control.Applicative hiding (empty)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Data
import           Data.Monoid
import           Data.Foldable       (Foldable (..))
import qualified Data.VPlan.At       as A
import           Data.VPlan.Class
import           Data.VPlan.TH
import           GHC.Generics

-- | This doesn't contain any value, it just ignores the s parameter (except for the IxValue/Ixed families)
data Empty (s :: * -> * -> * -> *) c i v = Empty deriving (Eq, Show, Generic, Read)
makeModifier ''Empty

instance Limited (Empty s c i v) where
  imax = const Nothing
  imin = const Nothing

deriving instance (Typeable3 s, Typeable c, Typeable i, Typeable v) => Data (Empty s c i v)

instance (Gettable f) => A.Contains f (Empty s c i v) where contains = containsTest $ const $ const False
instance (Applicative f) => A.Ixed f (Empty s c i v) where ix _ _ = pure
instance Functor (Empty s c i) where fmap _ Empty = Empty
instance Profunctor (Empty s c) where dimap _ _ Empty = Empty
instance Bifunctor (Empty s c) where bimap _ _ Empty = Empty
instance Contravariant (Empty s c i) where contramap _ Empty = Empty
instance FromJSON (Empty s c i v) where parseJSON _ = pure Empty
instance ToJSON (Empty s c i v) where toJSON _ = emptyObject
instance Foldable (Empty s c i) where fold _ = mempty
instance Traversable (Empty s c i) where traverse = const $ pure . coerce
instance (Enum (Index (s c i v)), Monoid (Index (s c i v))) => Periodic (Empty s c i v) where interval = const $ succ mempty
