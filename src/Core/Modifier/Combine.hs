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
import qualified Core.AtSansFunctor  as A

-- | Combine two modifiers into one. While traversal, results of the first modifier are traversed
-- first. After that, the results of the second modifier are traversed.
data Combine s = Combine s s deriving (Eq)

-- | Combine two modifiers. See 'Combine' for details.
combine :: e -> e -> Combine e
combine = Combine

type instance IxValue (Combine e) = IxValue e
type instance Index (Combine e) = Index e

instance (Gettable f, A.Contains (Accessor Bool) s) => A.Contains f (Combine s) where
  contains = containsTest $ \i (Combine a b) -> a ^. A.contains i || b ^. A.contains i

instance (A.Ixed f e, Applicative f) => A.Ixed f (Combine e) where
  ix i f (Combine a b) = liftA2 combine (A.ix i f a) (A.ix i f b)

instance (A.Ixed f (Combine e), Functor f) => Ixed f (Combine e) where
  ix = A.ix

instance (A.Contains f (Combine e), Functor f) => Contains f (Combine e) where
  contains = A.contains
