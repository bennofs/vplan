{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverlappingInstances   #-}
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
    (>||<)(R, L)
  , (>||)()
  , (||<)()
  , Close
  , enumValue
  , enumSchedule
  , enumItem
  , scheduleItem
  , MakeTypeEnum
  ) where

import Core.Schedule
import Core.Builder
import Data.Void
import Control.Applicative
import Control.Lens

-- | Either for types with one argument
data (>||<) a b s = L (a s) | R (b s)

-- | Just an alias to make writing instances easier
type C = (>||<)

-- | Just a little helper to make the types match
newtype Close a = Close Void

type instance IxValue (Close a) = IxValue a
type instance Index (Close a) = Index a

instance (Gettable f) => Contains f (Close a) where
  contains = containsTest (const $ const True)

instance (Functor f) => Ixed f (Close a) where
  ix _ _ (Close v) = absurd v

-- | Use this to concat the last element onto a type enum.
type a >|| b = a >||< b >||< Close

-- | Use this to concat the first element onto a type enum.
type a ||< b = a >||< b

infixr 7 ||<, >||, >||<

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
instance (Contains f (a s), Contains f (b s), Index (a s) ~ Index s, Index (b s) ~ Index s)
         => Contains f (C a b s) where
  contains i f (L x) = L <$> contains i f x
  contains i f (R x) = R <$> contains i f x

instance (Ixed f (a s), Ixed f (b s), Index (a s) ~ Index s, Index (b s) ~ Index s,
          IxValue (a s) ~ IxValue s, IxValue (b s) ~ IxValue s) => Ixed f (C a b s) where
  ix i f (L x) = L <$> ix i f x
  ix i f (R x) = R <$> ix i f x

-- | Build a value as a schedule containing an enum.
enumSchedule :: (MakeTypeEnum a (s (Schedule i v s))) => a -> Schedule i v s
enumSchedule = view schedule . enumValue

-- | Build an enum value as a single item.
enumItem :: (MakeTypeEnum a e) => a -> Builder e ()
enumItem = item . enumValue

-- | Build an enum value as a single schedule item.
scheduleItem :: (MakeTypeEnum a (s (Schedule i v s))) => a -> Builder (Schedule i v s) ()
scheduleItem = item . enumSchedule
