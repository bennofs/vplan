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
  , constant
  ) where

import           Control.Lens

-- | A modifier that always returns the same value, no matter to what it is applied.
newtype Constant s = Constant (IxValue s)
makeIso ''Constant

deriving instance (Eq (IxValue s)) => Eq (Constant s)

type instance IxValue (Constant s) = IxValue s
type instance Index (Constant s) = Index s

instance (Functor f) => Ixed f (Constant s) where
  ix i f = from constant $ indexed f i

instance (Gettable f) => Contains f (Constant s) where
  contains = containsTest $ const $ const True
