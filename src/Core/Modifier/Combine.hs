{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : $Header$
-- Description : A modifier that combines the result of several modifiers.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (Uses various GHC extensions)
module Core.Modifier.Combine (
    Combine (..)
  , combine
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Monoid

-- | Combine two modifiers into one. While traversal, results of the first modifier are traversed
-- first. After that, the results of the second modifier are traversed.
data Combine s = Combine s s deriving (Eq)

-- | Combine two modifiers. See 'Combine' for details.
combine :: e -> e -> Combine e
combine = Combine

type instance IxValue (Combine e) = IxValue e
type instance Index (Combine e) = Index e

instance (Gettable f, Ixed (Accessor Any) s) => Contains f (Combine s) where
  contains = containsIx

instance (Ixed (Accessor Any) e, Ixed f e, Applicative f, Functor f) => Ixed f (Combine e) where
  ix i f (Combine a b) = liftA2 combine (ix i f a) (ix i f b)
