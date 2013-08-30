{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | A modifier that references a value in a schedule
module Data.VPlan.Modifier.Reference (
    Reference(..)
  , source
  , reference
  , referenced
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

-- | Reference the value at index '_source' in the schedule '_referenced' and repeat it infinitely.
data Reference s i v = Reference {_source :: i, _referenced :: s i v} deriving (Eq, Generic)
makeLenses ''Reference
makeModifier ''Reference
deriveClass ''Reference

deriving instance (Show i, Show (s i v)) => Show (Reference s i v)
deriving instance (Read i, Read (s i v)) => Read (Reference s i v)
deriving instance (Data i, Data (s i v), Typeable2 s, Typeable i, Typeable v) => Data (Reference s i v)

-- | Construct a new reference.
reference :: i -> s i v -> Reference s i v
reference = Reference

instance (Eq i, i ~ Index (s i v), Gettable f, A.Contains f (s i v)) => A.Contains f (Reference s i v) where
  contains _ f r = referenced ?? r $ A.contains (r ^. source) f

instance (Eq i, i ~ Index (s i v), A.Ixed f (s i v), Functor f) => A.Ixed f (Reference s i v) where
  ix _ f r = referenced ?? r $ A.ix (r ^. source) f

instance Functor (s i) => Functor (Reference s i) where fmap f = referenced %~ fmap f
instance Bifunctor s => Bifunctor (Reference s) where bimap f g (Reference i u) = Reference (f i) $ bimap f g u
instance Contravariant (s i) => Contravariant (Reference s i) where contramap f = referenced %~ contramap f
instance Foldable (s i) => Foldable (Reference s i) where foldMap = views referenced . foldMap
instance Traversable (s i) => Traversable (Reference s i) where traverse = referenced . traverse

instance (FromJSON i, FromJSON (s i v)) => FromJSON (Reference s i v) where
  parseJSON (Object o) = Reference <$> o .: "index" <*> o .: "child"
  parseJSON v = typeMismatch "Object" v

instance (ToJSON i, ToJSON (s i v)) => ToJSON (Reference s i v) where
  toJSON (Reference i u) = object ["index" .= i, "child" .= u]
