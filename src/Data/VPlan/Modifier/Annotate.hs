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

-- | A modifier that allows to attach some additional static data to another modifier.
module Data.VPlan.Modifier.Annotate
  ( Annotate(..)
  , annotate
  , attached
  , annotated
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
import           GHC.Generics

-- | @Annotate a@ is a modifier that can attach data of type @a@ to some other modifier.
data Annotate a s c i v = Annotate
  { _attached  :: a         -- ^ Contains the attached value
  , _annotated :: s c i v   -- ^ Contains the modifier the value is attached to
  } deriving (Eq, Generic)

makeLenses ''Annotate
makeModifier ''Annotate

-- | Annotate another modifier with a value of type @a@.
annotate :: a -> s c i v -> Annotate a s c i v
annotate = Annotate

deriving instance (Data a, Data (s c i v), Typeable3 s, Typeable c, Typeable i, Typeable v) => Data (Annotate a s c i v)
deriving instance (Show (s c i v), Show a) => Show (Annotate a s c i v)
deriving instance (Read (s c i v), Read a) => Read (Annotate a s c i v)

instance (A.Contains f (s c i v), Functor f) => A.Contains f (Annotate a s c i v) where contains = fmap annotated . A.contains
instance (A.Ixed f (s c i v), Functor f) => A.Ixed f (Annotate a s c i v) where ix = fmap annotated . A.ix
instance Functor (s c i) => Functor (Annotate a s c i) where fmap f = annotated %~ fmap f
instance Bifunctor (s c) => Bifunctor (Annotate a s c) where bimap f g = annotated %~ bimap f g
instance Profunctor (s c) => Profunctor (Annotate a s c) where dimap f g = annotated %~ dimap f g
instance Contravariant (s c i) => Contravariant (Annotate a s c i) where contramap f = annotated %~ contramap f
instance Foldable (s c i) => Foldable (Annotate a s c i) where foldMap = views annotated . foldMap
instance Traversable (s c i) => Traversable (Annotate a s c i) where traverse = annotated . traverse
instance Periodic (s c i v) => Periodic (Annotate a s c i v) where interval = views annotated interval

instance Limited (s c i v) => Limited (Annotate a s c i v) where
  imin = views annotated imin
  imax = views annotated imax

instance (FromJSON (s c i v), FromJSON a) => FromJSON (Annotate a s c i v) where
  parseJSON (Object o) = Annotate <$> o .: "data" <*> o .: "child"
  parseJSON v = typeMismatch "Object" v

instance (ToJSON (s c i v), ToJSON a) => ToJSON (Annotate a s c i v) where
  toJSON (Annotate a c) = object [ "data" .= a, "child" .= c ]
