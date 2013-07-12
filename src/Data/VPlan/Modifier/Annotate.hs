{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | A modifier that allows to attach some additional static data to another modifier.
module Data.VPlan.Modifier.Annotate
  ( Annotate(..)
  , annotate
  , attached
  , annotated
  ) where

import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At    as A
import           Data.VPlan.Class
import           Data.VPlan.TH

-- | @Annotate a@ is a modifier that can attach data of type @a@ to some other modifier.
data Annotate a s i v = Annotate
  { _attached  :: a       -- ^ Contains the attached value
  , _annotated :: s i v   -- ^ Contains the modifier the value is attached to
  } deriving (Eq)

makeLenses ''Annotate
makeModifier ''Annotate
deriveClass ''Annotate

-- | Annotate another modifier with a value of type @a@.
annotate :: a -> (s i v) -> Annotate a s i v
annotate = Annotate

deriving instance (Data a, Data (s i v), Typeable2 s, Typeable i, Typeable v) => Data (Annotate a s i v)

instance (A.Contains f (s i v), Functor f) => A.Contains f (Annotate a s i v) where
  contains = fmap annotated . A.contains

instance (A.Ixed f (s i v), Functor f) => A.Ixed f (Annotate a s i v) where
  ix = fmap annotated . A.ix

instance (Functor (s i)) => Functor (Annotate a s i) where
  fmap f (Annotate a x) = Annotate a (fmap f x)

instance (Bifunctor s) => Bifunctor (Annotate a s) where
  bimap f g (Annotate a x) = Annotate a (bimap f g x)

instance (Profunctor s) => Profunctor (Annotate a s) where
  dimap f g (Annotate a x) = Annotate a (dimap f g x)
