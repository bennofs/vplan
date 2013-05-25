{-# LANGUAGE DeriveDataTypeable    #-}
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
module Data.VPlan.Modifier.Limit (
    Limit()
  , condition
  , bound
  , limited
  , lower
  , equal
  , greater
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At       as A
import           Data.VPlan.TH

-- | The 'Limit' modifier takes another modifier and behaves like that one, but only when the index compared to
-- the bound gives the 'condition'. If that's not the case, it behaves like the empty modifier.
data Limit s = Limit { _condition :: Ordering, _bound :: Index s, _limited :: s }
makeLenses ''Limit
makeModifier ''Limit

deriving instance (Data (Index s), Data s) => Data (Limit s)
deriving instance (Eq (Index s), Eq s) => Eq (Limit s)

instance (A.Contains f s, Ord (Index s), Gettable f) => A.Contains f (Limit s) where
  contains i f l
    | compare i (l ^. bound) == l ^. condition = limited (A.contains i f) l
    | otherwise = coerce $ indexed f i False

instance (A.Ixed f s, Applicative f, Ord (Index s)) => A.Ixed f (Limit s) where
  ix i f l
    | compare i (l ^. bound) == l ^. condition = limited (A.ix i f) l
    | otherwise = pure l

-- | Limit another schedule to all indices lower than the given one
lower :: Index s -> s -> Limit s
lower = Limit LT

-- | Pick only the value at the given index from anther schedule
equal :: Index s -> s -> Limit s
equal = Limit EQ

-- | Limit another schedule to all the indices hight than the given one
greater :: Index s -> s -> Limit s
greater = Limit GT
