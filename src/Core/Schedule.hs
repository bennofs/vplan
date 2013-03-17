{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : $Header$
-- Description : The schedule data type.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (uses various GHC extensions)
module Core.Schedule (
    Ignore(..)
  , Void
  , Schedule(..)
  , schedule
  , scheduleIndex
  ) where

import Core.Modifier
import Data.Monoid
import Data.Void
import Control.Lens

-- | Const at the type level. Ignores the second type parameter. Useful for Modifiers that don't
-- accept a type parameter.
newtype Ignore a b = Ignore a

instance (Monoid v) => Modifier (Ignore Void c) i v p where
  modifierApply  (Ignore v) = absurd v
  modifierPeriod (Ignore v) = absurd v

-- | The type of a schedule. This type is just fix at the type level.
newtype Schedule e = Schedule (e (Schedule e))
makeIso ''Schedule

-- | Index into a 'Schedule'
scheduleIndex :: (Modifier (e (Schedule e)) i v p) => Schedule e -> i -> v
scheduleIndex s = modifierApply (review schedule s) (const mempty)
