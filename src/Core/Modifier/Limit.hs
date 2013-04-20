{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
import qualified Core.AtSansFunctor as A

data Limit s = Limit { _condition :: Ordering, _bound :: Index s, _underlying :: s }
makeLenses ''Limit

deriving instance (Eq (Index s), Eq s) => Eq (Limit s)

type instance IxValue (Limit s) = IxValue s
type instance Index (Limit s) = Index s

instance (A.Contains f s, Ord (Index s), Gettable f) => A.Contains f (Limit s) where
  contains i f l
    | compare i (l ^. bound) == l ^. condition = underlying (A.contains i f) l
    | otherwise = coerce $ indexed f i False

instance (A.Ixed f s, Applicative f, Ord (Index s)) => A.Ixed f (Limit s) where
  ix i f l
    | compare i (l ^. bound) == l ^. condition = underlying (A.ix i f) l
    | otherwise = pure l

instance (A.Contains f (Limit s), Functor f) => Contains f (Limit s) where
  contains = A.contains

instance (A.Ixed f (Limit s), Functor f) => Ixed f (Limit s) where
  ix = A.ix

lower :: Index s -> s -> Limit s
lower = Limit LT

equal :: Index s -> s -> Limit s
equal = Limit EQ

greater :: Index s -> s -> Limit s
greater = Limit GT
