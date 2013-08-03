{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | A modifier that combines the result of several modifiers.
module Data.VPlan.Modifier.Combine (
    Combine (..)
  , combine
  ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Data
import           Data.Foldable       (Foldable (..))
import qualified Data.VPlan.At       as A
import           Data.VPlan.Class
import           Data.VPlan.TH

-- | Combine multiple modifiers into one. Values are traversed in the order of the modifers, i.e. the
-- values of the first modifier are traversed first.
data Combine s i v = Combine [s i v] deriving (Eq)
makeModifier ''Combine
deriveClass ''Combine

deriving instance (Data (s i v), Typeable2 s, Typeable i, Typeable v) => Data (Combine s i v)

-- | Combine multiple modifiers. See 'Combine' for details.
combine :: [s i v] -> Combine s i v
combine = Combine

instance (Bifunctor s) => Bifunctor (Combine s) where
  bimap f g (Combine a) = Combine $ fmap (bimap f g) a

instance (Functor (s i)) => Functor (Combine s i) where
  fmap f (Combine a) = Combine (fmap (fmap f) a)

instance (Profunctor s) => Profunctor (Combine s) where
  dimap l r (Combine a) = Combine (fmap (dimap l r) a)

instance (Gettable f, A.Contains (Accessor Bool) (s i v)) => A.Contains f (Combine s i v) where
  contains = containsTest $ \i (Combine a) -> any (view $ A.contains i) a

instance (A.Ixed f (s i v), Applicative f) => A.Ixed f (Combine s i v) where
  ix i f (Combine a) = Combine <$> traverse (A.ix i f) a

instance (Foldable (s i)) => Foldable (Combine s i) where
  fold (Combine a) = foldMap fold a

instance (Traversable (s i)) => Traversable (Combine s i) where
  traverse f (Combine a) = Combine <$> traverse (traverse f) a

instance FromJSON (s i v) => FromJSON (Combine s i v) where
  parseJSON (Object o) = Combine <$> o .: "childs"
  parseJSON v = typeMismatch "Object" v

instance ToJSON (s i v) => ToJSON (Combine s i v) where
  toJSON (Combine a) = object [ "childs" .= a ]
