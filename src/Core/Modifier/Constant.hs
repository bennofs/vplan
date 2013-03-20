{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
-- |
-- Module      : $Header$
-- Description : A modifier that always returns a constant value.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable

module Core.Modifier.Constant (
    Constant(..)
  , constant
  ) where

import Core.Modifier
import Data.Monoid
import Control.Lens
import Control.Applicative

-- | A modifier that always returns the same value, no matter to what it is applied.
newtype Constant a b = Constant a deriving (Functor, Show)
makeIso ''Constant

instance (Monoid m) => Applicative (Constant m) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant $ x <> y

instance (Monoid a) => Modifier (Constant a b) i a where
  modifierApply (Constant a) _ = const a
