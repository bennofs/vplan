{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At       as A
import           Data.VPlan.Class
import           Data.VPlan.TH

-- | Combine two modifiers into one. On traversal, results of the first modifier are traversed
-- first. After that, the results of the second modifier are traversed.
data Combine s i v = Combine (s i v) (s i v) deriving (Eq)
makeModifier ''Combine
deriveClass ''Combine

deriving instance (Data (s i v), Typeable2 s, Typeable i, Typeable v) => Data (Combine s i v)

-- | Combine two modifiers. See 'Combine' for details.
combine :: s i v -> s i v -> Combine s i v
combine = Combine

instance (Bifunctor s) => Bifunctor (Combine s) where
  bimap f g (Combine a b) = Combine (bimap f g a) (bimap f g b)

instance (Functor (s i)) => Functor (Combine s i) where
  fmap f (Combine a b) = Combine (fmap f a) (fmap f b)

instance (Profunctor s) => Profunctor (Combine s) where
  dimap l r (Combine a b) = Combine (dimap l r a) (dimap l r b)

instance (Gettable f, A.Contains (Accessor Bool) (s i v)) => A.Contains f (Combine s i v) where
  contains = containsTest $ \i (Combine a b) -> a ^. A.contains i || b ^. A.contains i

instance (A.Ixed f (s i v), Applicative f) => A.Ixed f (Combine s i v) where
  ix i f (Combine a b) = liftA2 combine (A.ix i f a) (A.ix i f b)
