{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : $Header$
-- Description : Limit a modifier with some predicate on the index
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable
module Core.Modifier.Limit (
    Limit()
  , lower
  , equal
  , greater
  ) where
import           Core.Modifier
import           Data.Monoid

data Limit i e = Limit Ordering i

instance (Ord i, Monoid v) => Modifier (Limit i e) i v where
  modifierApply (Limit o l) f i
    | compare i l == o = f i
    | otherwise = mempty

lower :: i -> Limit i e
lower = Limit LT

equal :: i -> Limit i e
equal = Limit EQ

greater :: i -> Limit i e
greater = Limit GT
