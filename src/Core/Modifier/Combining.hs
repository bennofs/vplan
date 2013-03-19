{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
-- |
-- Module      : $Header$
-- Description : A modifier that wraps another modifier, combining the result of the modifier with
--               the old result using the monoid append function.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (Uses various GHC extensions)
module Core.Modifier.Combining (
    Combining ()
  , combining
  ) where

import Core.Modifier
import Control.Lens

newtype Combining e = Combining e
makeIso ''Combining

instance (Modifier e i v) => Modifier (Combining e) i v where
  modifierApply (Combining e) f = f <> modifierApply e f
