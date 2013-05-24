{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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

import           Control.Applicative hiding (empty)
import           Control.Lens
import qualified Core.AtSansFunctor  as A
import           Data.Data

data Empty s = Empty deriving (Eq, Data)
instance Typeable1 Empty where
  typeOf1 _ = mkTyCon3 "vplan-utils" "Core.Modifier.Empty" "Empty" `mkTyConApp` []

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
