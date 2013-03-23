{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
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
    Limit(..)
  , lower
  , equal
  , greater
  ) where

import           Control.Applicative
import           Control.Lens

data Limit s = Limit { _condition :: Ordering, _bound :: Index s, _underlying :: s }
makeLenses ''Limit

type instance IxValue (Limit s) = IxValue s
type instance Index (Limit s) = Index s

instance (Contains f s, Ord (Index s), Gettable f) => Contains f (Limit s) where
  contains i f l
    | compare i (l ^. bound) == l ^. condition = underlying (contains i f) l
    | otherwise = coerce $ indexed f i False

instance (Ixed f s, Applicative f, Ord (Index s)) => Ixed f (Limit s) where
  ix i f l
    | compare i (l ^. bound) == l ^. condition = underlying (ix i f) l
    | otherwise = pure l

lower :: Index s -> s -> Limit s
lower = Limit LT

equal :: Index s -> s -> Limit s
equal = Limit EQ

greater :: Index s -> s -> Limit s
greater = Limit GT
