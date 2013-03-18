{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE StandaloneDeriving    #-}
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
    Schedule(..)
  , schedule
  , scheduleIndex
  ) where

import Core.Modifier
import Data.Monoid
import Control.Lens

-- | The type of a schedule. This type is just fix at the type level.
newtype Schedule e = Schedule (e (Schedule e))
makeIso ''Schedule

instance (Modifier (e (Schedule e)) i v) => Modifier (Schedule e) i v where
  modifierApply (Schedule s) = modifierApply s

deriving instance (Show (e (Schedule e))) => Show (Schedule e)

-- | Index into a 'Schedule'
scheduleIndex :: (Modifier (e (Schedule e)) i v) => Schedule e -> i -> v
scheduleIndex s = modifierApply (review schedule s) (const mempty)
