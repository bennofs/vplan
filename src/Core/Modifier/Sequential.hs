{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : $Header$
-- Description : A modifier that allows to execute some modifiers in sequence, one after another.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (Uses various GHC extensions)
module Core.Modifier.Sequential (
  Sequential(),
  sequential
  ) where

import qualified Data.Sequence as S
import Data.Foldable
import Prelude hiding (concatMap, concat)
import Core.Modifier
import Core.Time
import Control.Lens

newtype Sequential e = Sequential (S.Seq e)
makeIso ''Sequential

instance (Modifier e i v) => Modifier (Sequential e) i v where
  modifierApply s f = foldl' (flip modifierApply) f $ s ^. from sequential
