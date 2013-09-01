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
data Reference s c i v = Reference {_source :: i, _referenced :: s c i v} deriving (Eq, Generic)
makeLenses ''Reference
makeModifier ''Reference
deriveClass ''Reference

deriving instance (Show i, Show (s c i v)) => Show (Reference s c i v)
deriving instance (Read i, Read (s c i v)) => Read (Reference s c i v)
deriving instance (Data i, Data (s c i v), Typeable3 s, Typeable c, Typeable i, Typeable v) => Data (Reference s c i v)

-- | Construct a new reference.
reference :: i -> s c i v -> Reference s c i v
reference = Reference

instance (Eq i, i ~ Index (s c i v), Gettable f, A.Contains f (s c i v)) => A.Contains f (Reference s c i v) where
  contains _ f r = referenced ?? r $ A.contains (r ^. source) f

instance (Eq i, i ~ Index (s c i v), A.Ixed f (s c i v), Functor f) => A.Ixed f (Reference s c i v) where
  ix _ f r = referenced ?? r $ A.ix (r ^. source) f

instance Functor (s c i) => Functor (Reference s c i) where fmap f = referenced %~ fmap f
instance Bifunctor (s c) => Bifunctor (Reference s c) where bimap f g (Reference i u) = Reference (f i) $ bimap f g u
instance Contravariant (s c i) => Contravariant (Reference s c i) where contramap f = referenced %~ contramap f
instance Foldable (s c i) => Foldable (Reference s c i) where foldMap = views referenced . foldMap
instance Traversable (s c i) => Traversable (Reference s c i) where traverse = referenced . traverse

instance (FromJSON i, FromJSON (s c i v)) => FromJSON (Reference s c i v) where
  parseJSON (Object o) = Reference <$> o .: "index" <*> o .: "child"
  parseJSON v = typeMismatch "Object" v

instance (ToJSON i, ToJSON (s c i v)) => ToJSON (Reference s c i v) where
  toJSON (Reference i u) = object ["index" .= i, "child" .= u]
