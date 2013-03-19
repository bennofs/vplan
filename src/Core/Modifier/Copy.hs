{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
-- |
-- Module      : $Header$
-- Description : A modifier that "copies" a value from one index to another one.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (Uses various GHC extensions)
module Core.Modifier.Copy (
    Copy()
  , target
  , source
  , copy
  ) where

import Core.Modifier
import Data.Monoid
import Control.Lens

data Copy i e = Copy {_copySource :: i, _copyTarget :: i}
makeFields ''Copy

copy :: i -> i -> Copy i e
copy = Copy

instance (Monoid v, Eq i) => Modifier (Copy i e) i v where
  modifierApply c f i
    | i == c ^. target = f $ c ^. source
    | otherwise = f i
