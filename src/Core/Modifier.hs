{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
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
  , modifierPeriod
  , (|<>|)
  ) where

import Data.Monoid
import Control.Applicative

-- | A class for a modifier. A modifier can modify an index-function from index i to monoid values
-- v. A modifier may have a period of type p, but this is optional.
class (Monoid v) => Modifier a i v p | a -> i, a -> v, a -> p where

  -- | Apply the modifier to an indexing function.
  modifierApply :: a -> (i -> v) -> i -> v

  -- | Get the period of the modifier.
  modifierPeriod :: a -> Maybe b

-- | A lifted mappend.
(|<>|) :: (Monoid a, Applicative m) => m a -> m a -> m a
(|<>|) = liftA2 (<>)
