{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-} -- Required for (~)
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
-- |
-- Module      : $Header$
-- Description : A modifier that can contain exactly one out of multiple modifiers of possible
--                different types.
-- Copyright   : (c) Benno Fünfstücknnn
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (uses various GHC-specific extensions)
module Core.Modifier.Enum (
    (>||<)()
  , (>||)()
  , enumValue
  , enumItem
  , scheduleItem
  ) where

import Core.Schedule
import Core.Modifier
import Core.Builder
import Control.Lens

-- | Either for types with one argument
data (>||<) a b c = L (a c) | R (b c)

-- | Use this to concat the last element onto a type enum.
type a >|| b = a >||< b >||< Ignore Void

-- | Create a type enum with a given value.
class MakeTypeEnum a b where

  -- | Create an enum with the given value.
  enumValue :: a -> b

instance (c ~ d) => MakeTypeEnum (a d) ((a >||< b) c) where
  enumValue = L

instance (MakeTypeEnum a (r c)) => MakeTypeEnum a ((l >||< r) c) where
  enumValue = R . enumValue

instance MakeTypeEnum a ((Ignore a >||< b) c) where
  enumValue = L . Ignore

instance (Modifier (l s) i v p, Modifier (r s) i v p) => Modifier ((l >||< r) s) i v p where
  modifierApply  (L a) = modifierApply a
  modifierApply  (R b) = modifierApply b
  modifierPeriod (L a) = modifierPeriod a
  modifierPeriod (R b) = modifierPeriod b

-- | Build an enum value as a single item.
enumItem :: (MakeTypeEnum a e) => a -> Builder e ()
enumItem = item . enumValue

-- | Build an enum value as a single schedule item.
scheduleItem :: (MakeTypeEnum a (e (Schedule e))) => a -> Builder (Schedule e) ()
scheduleItem = item . view schedule . enumValue
