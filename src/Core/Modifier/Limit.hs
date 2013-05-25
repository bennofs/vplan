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
module Core.Modifier.Limit (
    Limit(..)
  , lower
  , equal
  , greater
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Core.AtSansFunctor  as A
import           Core.TH
import           Data.Data

data Limit s = Limit { _condition :: Ordering, _bound :: Index s, _underlying :: s }
makeLenses ''Limit
makeModifier ''Limit

deriving instance (Data (Index s), Data s) => Data (Limit s)
deriving instance (Eq (Index s), Eq s) => Eq (Limit s)

instance (A.Contains f s, Ord (Index s), Gettable f) => A.Contains f (Limit s) where
  contains i f l
    | compare i (l ^. bound) == l ^. condition = underlying (A.contains i f) l
    | otherwise = coerce $ indexed f i False

instance (A.Ixed f s, Applicative f, Ord (Index s)) => A.Ixed f (Limit s) where
  ix i f l
    | compare i (l ^. bound) == l ^. condition = underlying (A.ix i f) l
    | otherwise = pure l

lower :: Index s -> s -> Limit s
lower = Limit LT

equal :: Index s -> s -> Limit s
equal = Limit EQ

greater :: Index s -> s -> Limit s
greater = Limit GT
