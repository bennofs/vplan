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
-- Description : A modifier that always returns a constant value.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable

module Core.Modifier.Constant (
    Constant(..)
  , constant  ) where

import           Control.Lens
import qualified Core.AtSansFunctor as A
import           Data.Data

-- | A modifier that always returns the same value, no matter to what it is applied.
newtype Constant s = Constant (IxValue s)
instance Typeable1 Constant where
  typeOf1 _ = mkTyCon3 "vplan-utils" "Core.Modifier.Constant" "Constant" `mkTyConApp` []
makeIso ''Constant

deriving instance (Data (IxValue s), Data s) => Data (Constant s)
deriving instance (Eq (IxValue s)) => Eq (Constant s)

type instance IxValue (Constant s) = IxValue s
type instance Index (Constant s) = Index s

instance (Gettable f) => A.Contains f (Constant s) where
  contains = containsTest $ const $ const True

instance (Functor f) => A.Ixed f (Constant s) where
  ix i f = from constant $ indexed f i

instance (A.Contains f (Constant s), Functor f) => Contains f (Constant s) where
  contains = A.contains

instance (A.Ixed f (Constant s), Functor f) => Ixed f (Constant s) where
  ix = A.ix
