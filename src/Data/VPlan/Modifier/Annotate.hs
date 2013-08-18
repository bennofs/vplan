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
data Annotate a s i v = Annotate
  { _attached  :: a       -- ^ Contains the attached value
  , _annotated :: s i v   -- ^ Contains the modifier the value is attached to
  } deriving (Eq, Generic)

makeLenses ''Annotate
makeModifier ''Annotate
deriveClass ''Annotate

-- | Annotate another modifier with a value of type @a@.
annotate :: a -> (s i v) -> Annotate a s i v
annotate = Annotate

deriving instance (Data a, Data (s i v), Typeable2 s, Typeable i, Typeable v) => Data (Annotate a s i v)
deriving instance (Show (s i v), Show a) => Show (Annotate a s i v)

instance (A.Contains f (s i v), Functor f) => A.Contains f (Annotate a s i v) where contains = fmap annotated . A.contains
instance (A.Ixed f (s i v), Functor f) => A.Ixed f (Annotate a s i v) where ix = fmap annotated . A.ix
instance (Functor (s i)) => Functor (Annotate a s i) where fmap f = annotated %~ fmap f
instance (Bifunctor s) => Bifunctor (Annotate a s) where bimap f g = annotated %~ bimap f g
instance (Profunctor s) => Profunctor (Annotate a s) where dimap f g = annotated %~ dimap f g
instance (Contravariant (s i)) => Contravariant (Annotate a s i) where contramap f = annotated %~ contramap f
instance (Foldable (s i)) => Foldable (Annotate a s i) where fold = views annotated fold
instance (Traversable (s i)) => Traversable (Annotate a s i) where traverse = annotated . traverse

instance (FromJSON (s i v), FromJSON a) => FromJSON (Annotate a s i v) where
  parseJSON (Object o) = Annotate <$> o .: "data" <*> o .: "child"
  parseJSON v = typeMismatch "Object" v

instance (ToJSON (s i v), ToJSON a) => ToJSON (Annotate a s i v) where
  toJSON (Annotate a c) = object [ "data" .= a, "child" .= c ]
