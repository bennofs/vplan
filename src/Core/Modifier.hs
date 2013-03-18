{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : $Header$
-- Description : The type class for modifiers and basic utilies for building modifiers.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (MultiParamTypeClasses and FunctionalDependencies)
module Core.Modifier (
    Modifier
  , modifierApply
  , (|<>|)
  , alternative2
  ) where

import Data.Monoid
import Control.Applicative

-- | A class for a modifier. A modifier can modify an index-function from index i to monoid values
-- v. A modifier may have a period of type p, but this is optional.
class (Monoid v) => Modifier a i v where

  -- | Apply the modifier to an indexing function.
  modifierApply :: a -> (i -> v) -> i -> v

-- | A lifted mappend.
(|<>|) :: (Monoid a, Applicative m) => m a -> m a -> m a
(|<>|) = liftA2 (<>)

-- | Apply a function to two Alternatives, and if either one is empty, return the other one.
alternative2 :: (Alternative f) => (a -> a -> a) -> f a -> f a -> f a
alternative2 f a b = liftA2 f a b <|> a <|> b
