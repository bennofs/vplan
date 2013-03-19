{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-} -- Required for (~)
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
    (>||<)(L,R)
  , (>||)()
  , Close
  , enumValue
  , enumSchedule
  , enumItem
  , scheduleItem
  ) where

import Core.Schedule
import Core.Modifier
import Core.Builder
import Data.Void
import Data.Monoid
import Control.Lens

-- | Either for types with one argument
data (>||<) a b c = L (a c) | R (b c) deriving (Show)

-- | Just a little helper to make the types match
newtype Close a = Close Void deriving (Show)

-- | Use this to concat the last element onto a type enum.
type a >|| b = a >||< b >||< Close
infixr 7 >||, >||<

-- | Create a type enum with a given value.
class MakeTypeEnum a b where

  -- | Create an enum with the given value.
  enumValue :: a -> b

instance (d ~ c) => MakeTypeEnum (a d) ((a >||< b) c) where
  enumValue = L

instance (MakeTypeEnum a (r c)) => MakeTypeEnum a ((l >||< r) c) where
  enumValue = R . enumValue

instance (Monoid v) => Modifier (Close c) i v where
  modifierApply (Close x) = absurd x

instance (Modifier (l s) i v, Modifier (r s) i v) => Modifier ((l >||< r) s) i v where
  modifierApply  (L a) = modifierApply a
  modifierApply  (R b) = modifierApply b

-- | Build a value as a schedule containing an enum.
enumSchedule :: (MakeTypeEnum a (e (Schedule e))) => a -> Schedule e
enumSchedule = view schedule . enumValue

-- | Build an enum value as a single item.
enumItem :: (MakeTypeEnum a e) => a -> Builder e ()
enumItem = item . enumValue

-- | Build an enum value as a single schedule item.
scheduleItem :: (MakeTypeEnum a (e (Schedule e))) => a -> Builder (Schedule e) ()
scheduleItem = item . enumSchedule
