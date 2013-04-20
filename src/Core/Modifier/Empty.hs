{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
-- |
-- Module      : $Header$
-- Description : A modifier that contains no value.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable

module Core.Modifier.Empty (
    Empty(..)
  ) where

import Control.Applicative hiding (empty)
import Control.Lens
import qualified Core.AtSansFunctor as A

data Empty s = Empty deriving (Eq)

type instance IxValue (Empty s) = IxValue s
type instance Index (Empty s) = Index s

instance (Gettable f) => A.Contains f (Empty s) where
  contains = containsTest $ const $ const False

instance (Applicative f) => A.Ixed f (Empty s) where
  ix _ _ = pure

instance (A.Contains f (Empty s), Functor f) => Contains f (Empty s) where
  contains = A.contains

instance (A.Ixed f (Empty s), Functor f) => Ixed f (Empty s) where
  ix = A.ix
