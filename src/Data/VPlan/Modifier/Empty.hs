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
import           Data.Foldable       (Foldable (..))
import           Data.Monoid
import qualified Data.VPlan.At       as A
import           Data.VPlan.Class
import           Data.VPlan.TH
import           GHC.Generics

-- | This doesn't contain any value, it just ignores the s parameter (except for the IxValue/Ixed families)
data Empty (s :: * -> * -> *) i v = Empty deriving (Eq, Show, Generic, Read)
makeModifier ''Empty
deriveClass ''Empty

deriving instance (Typeable2 s, Typeable i, Typeable v) => Data (Empty s i v)

instance (Gettable f) => A.Contains f (Empty s i v) where contains = containsTest $ const $ const False
instance (Applicative f) => A.Ixed f (Empty s i v) where ix _ _ = pure
instance Functor (Empty s i) where fmap _ Empty = Empty
instance Profunctor (Empty s) where dimap _ _ Empty = Empty
instance Bifunctor (Empty s) where bimap _ _ Empty = Empty
instance Contravariant (Empty s i) where contramap _ Empty = Empty
instance FromJSON (Empty s i v) where parseJSON _ = pure Empty
instance ToJSON (Empty s i v) where toJSON _ = emptyObject
instance Foldable (Empty s i) where fold = mempty
instance Traversable (Empty s i) where traverse = const $ pure . coerce
