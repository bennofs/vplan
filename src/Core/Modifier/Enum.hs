{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : $Header$
-- Description : A modifier that can contain exactly one out of multiple modifiers of possible
--               different types.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (uses various GHC-specific extensions)
module Core.Modifier.Enum (
    (:><:)(R, L)
  , Close(..)
  , enumValue
  , enumSchedule
  , enumItem
  , scheduleItem
  , MakeTypeEnum
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Core.AtSansFunctor  as A
import           Core.Builder
import           Core.Schedule
import           Data.Void

-- | An Either for types with one type argument (which is passed to both sides)
data (:><:) a b s = L (a s) | R (b s) deriving (Eq)

-- | Sometimes, this reads better than the infix version
type C = (:><:)

-- | This type signalizes the end of a chain of (:><:)'s.
data Close a = Close Void deriving (Eq)

type instance Index (Close a) = Index a
type instance IxValue (Close a) = IxValue a

instance A.Contains f (Close a) where
  contains _ _ (Close v) = absurd v

instance A.Ixed f (Close a) where
  ix _ _ (Close v) = absurd v

instance (A.Contains f (Close a), Functor f) => Contains f (Close a) where
  contains = A.contains

instance (A.Ixed f (Close a), Functor f) => Ixed f (Close a) where
  ix = A.ix

infixr 7 :><:

-- | Create a type enum with a given value.
class MakeTypeEnum a b where

  -- | Create an enum with the given value.
  enumValue :: a -> b

instance (s ~ s') => MakeTypeEnum (a s') (C a b s) where
  enumValue = L

instance (MakeTypeEnum c (b s)) => MakeTypeEnum c (C a b s) where
  enumValue = R . enumValue

type instance Index (C a b s) = Index s
type instance IxValue (C a b s) = IxValue s
instance (A.Contains f (a s), A.Contains f (b s), Index (a s) ~ Index s, Index (b s) ~ Index s,
          Functor f) => A.Contains f (C a b s) where
  contains i f (L x) = L <$> A.contains i f x
  contains i f (R x) = R <$> A.contains i f x

instance (A.Ixed f (a s), Functor f, A.Ixed f (b s), Index (a s) ~ Index s, Index (b s) ~ Index s,
          IxValue (a s) ~ IxValue s, IxValue (b s) ~ IxValue s) => A.Ixed f (C a b s) where
  ix i f (L x) = L <$> A.ix i f x
  ix i f (R x) = R <$> A.ix i f x

instance (A.Contains f (C a b s), Functor f) => Contains f (C a b s) where
  contains = A.contains

instance (A.Ixed f (C a b s), Functor f) => Ixed f (C a b s) where
  ix = A.ix


-- | Build a value as a schedule containing an enum.
enumSchedule :: (MakeTypeEnum a (s (Schedule i v s))) => a -> Schedule i v s
enumSchedule = view schedule . enumValue

-- | Build an enum value as a single item.
enumItem :: (MakeTypeEnum a e) => a -> Builder e ()
enumItem = item . enumValue

-- | Build an enum value as a single schedule item.
scheduleItem :: (MakeTypeEnum a (s (Schedule i v s))) => a -> Builder (Schedule i v s) ()
scheduleItem = item . enumSchedule
