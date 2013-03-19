{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
-- |
-- Module      : $Header$
-- Description : A modifier that allows to apply several modifiers to the same input function,
--               combining the results.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable
module Core.Modifier.Parallel (
    Parallel()
  , parallel
  ) where

import Core.Modifier
import Control.Lens
import qualified Data.Sequence as S

newtype Parallel m = Parallel (S.Seq m)
makeIso ''Parallel

instance (Modifier m i v) => Modifier (Parallel m) i v where
  modifierApply = flip $ foldMapOf (re parallel . traverse) . flip modifierApply
