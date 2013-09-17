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
import           Data.Foldable       (Foldable (..), foldl')
import           Data.Monoid
import qualified Data.VPlan.At       as A
import           Data.VPlan.Class
import           Data.VPlan.TH
import           Data.VPlan.Util
import           GHC.Generics

-- | Combine multiple modifiers into one. Values are traversed in the order of the modifers, i.e. the
-- values of the first modifier are traversed first.
newtype Combine s c i v = Combine [s c i v] deriving (Eq, Generic)
makeIso ''Combine
makeModifier ''Combine

deriving instance Show (s c i v) => Show (Combine s c i v)
deriving instance Read (s c i v) => Read (Combine s c i v)
deriving instance (Data (s c i v), Typeable3 s, Typeable c, Typeable i, Typeable v) => Data (Combine s c i v)

instance Bifunctor (s c) => Bifunctor (Combine s c) where bimap f g (Combine a) = Combine $ fmap (bimap f g) a
instance Functor (s c i) => Functor (Combine s c i) where fmap f (Combine a) = Combine (fmap (fmap f) a)
instance Profunctor (s c) => Profunctor (Combine s c) where dimap l r (Combine a) = Combine (fmap (dimap l r) a)
instance Contravariant (s c i) => Contravariant (Combine s c i) where contramap f (Combine l) = Combine $ map (contramap f) l
instance Foldable (s c i) => Foldable (Combine s c i) where foldMap f (Combine a) = foldMap (foldMap f) a
instance Traversable (s c i) => Traversable (Combine s c i) where traverse f (Combine a) = Combine <$> traverse (traverse f) a
                                                                  
instance (Limited (s c i v), Ord (Index (s c i v))) => Limited (Combine s c i v) where
  imax (Combine as) = maximum <$> traverse imax as
  imin (Combine as) = minimum <$> traverse imin as

instance (Enum (Index (s c i v)), Monoid (Index (s c i v)), Ord (Index (s c i v)), Periodic (s c i v)) => Periodic (Combine s c i v) where
  interval (Combine []) = succ mempty
  interval (Combine (a:as)) = foldl' glcm (interval a) $ map interval as

instance (Gettable f, A.Contains (Accessor Bool) (s c i v)) => A.Contains f (Combine s c i v) where
  contains = containsTest $ \i (Combine a) -> any (view $ A.contains i) a

instance (A.Ixed f (s c i v), Applicative f) => A.Ixed f (Combine s c i v) where
 ix i f (Combine a) = Combine <$> traverse (A.ix i f) a

instance FromJSON (s c i v) => FromJSON (Combine s c i v) where
  parseJSON (Object o) = Combine <$> o .: "childs"
  parseJSON v = typeMismatch "Object" v

instance ToJSON (s c i v) => ToJSON (Combine s c i v) where
  toJSON (Combine a) = object [ "childs" .= a ]
