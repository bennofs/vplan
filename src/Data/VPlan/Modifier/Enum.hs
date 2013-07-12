{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- | A modifier that can contain exactly one out of multiple modifiers of different types.
module Data.VPlan.Modifier.Enum (
    (:><:)(R,L)
  , enumSchedule
  , enumItem
  , scheduleItem
  , EnumContains(enumValue)
  , EnumApply(enumApply)
  , CFunc(..)
  ) where

import           Control.Applicative
import           GHC.Exts
import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At       as A
import           Data.VPlan.Builder
import           Data.VPlan.Class
import           Data.VPlan.Schedule
import           Data.VPlan.TH

-- | An Either for types with one type argument (which is passed to both sides)
data (:><:) a b (s :: * -> * -> *) i v = L (a s i v) | R (b s i v) deriving (Eq)
infixr 7 :><:

makeModifier ''(:><:)

deriving instance (Typeable v, Typeable i, BothInstance Data a b s i v, Typeable2 (C a b s)) => Data ((:><:) a b s i v)

-- | Shorter alias
type C = (:><:)

-- | Require that a type enum can contain the given value
class EnumContains a b where

  -- | Create an enum with the given value.
  enumValue :: a s i v -> b (s :: * -> * -> *) i v

instance                       EnumContains a a       where enumValue = id
instance                       EnumContains a (C a b) where enumValue = L
instance                       EnumContains b (C a b) where enumValue = R
instance (EnumContains c  b) => EnumContains c (C a b) where enumValue = R . enumValue

type BothInstance (c :: * -> Constraint) a b (s :: * -> * -> *) i v = (c (a s i v), c (b s i v))
type BothInstance1 (c :: (* -> *) -> Constraint) a b (s :: * -> * -> *) i = (c (a s i), c (b s i))
type BothInstance2 (c :: (* -> * -> *) -> Constraint) a b (s:: * -> * -> *) = (c (a s), c (b s))
type BothSame f a b s i v = (f (C a b (s :: * -> * -> *) i v) ~ f (a s i v), f (C a b s i v) ~ f (b s i v))

instance (BothInstance (A.Contains f) a b s i v, BothSame Index a b s i v, Functor f) => A.Contains f (C a b s i v) where
  contains i f (L x) = L <$> A.contains i f x
  contains i f (R x) = R <$> A.contains i f x

instance (Functor f, BothInstance (A.Ixed f) a b s i v, BothSame Index a b s i v, BothSame IxValue a b s i v) => A.Ixed f (C a b s i v) where
  ix i f (L x) = L <$> A.ix i f x
  ix i f (R x) = R <$> A.ix i f x

instance (BothInstance Periodic a b s i v, BothSame Index a b s i v) => Periodic (C a b s i v) where
  interval (L a) = interval a
  interval (R a) = interval a

instance (BothInstance Limited a b s i v, BothSame Index a b s i v) => Limited (C a b s i v) where
  imin (L a) = imin a
  imin (R a) = imin a
  imax (L a) = imax a
  imax (R a) = imax a

instance (BothInstance1 Functor a b s i) => Functor (C a b s i) where
  fmap f (L x) = L $ fmap f x
  fmap f (R x) = R $ fmap f x

instance (BothInstance2 Bifunctor a b s) => Bifunctor (C a b s) where
  bimap f g (L x) = L $ bimap f g x
  bimap f g (R x) = R $ bimap f g x

instance (BothInstance2 Profunctor a b s) => Profunctor (C a b s) where
  dimap l r (L x) = L $ dimap l r x
  dimap l r (R x) = R $ dimap l r x

-- | A polymorphic, constrained function returning some type r.
data CFunc ctx r = CFunc
  { cfunc :: (forall a. ctx a => a -> r)
  }

-- | Class to which allows to apply functions to the values in an Enum
class EnumApply ctx e where
  enumApply :: CFunc ctx b -> e -> b

instance (ctx (l s i v), EnumApply ctx (r s i v)) => EnumApply ctx (C l r s i v) where
  enumApply f (L a) = cfunc f a
  enumApply f (R a) = enumApply f a

instance (ctx a) => EnumApply ctx a where
  enumApply f a = cfunc f a

-- | Build a value as a schedule containing an enum.
enumSchedule :: (EnumContains a s) => a (Schedule s) i v -> Schedule s i v
enumSchedule = view schedule . enumValue

-- | Build an enum value as a single item.
enumItem :: (EnumContains a e) => a (Schedule s) i v -> Builder (e (Schedule s) i v) ()
enumItem = item . enumValue

-- | Build an enum value as a single schedule item.
scheduleItem :: (EnumContains a s) => a (Schedule s) i v -> Builder (Schedule s i v) ()
scheduleItem = item . enumSchedule

instance (EnumContains m s) => Supported m (Schedule s) where new = enumSchedule
