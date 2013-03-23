{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : $Header$
-- Description : A modifier that "copies" a value from one index to another one.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (Uses various GHC extensions)
module Core.Modifier.Copy (
    Copy(..)
  , target
  , source
  , copy
  ) where

import           Control.Lens

data Copy s = Copy {_source :: Index s, _target :: Index s, _underlying :: s}
makeLenses ''Copy

copy :: Index s -> Index s -> s -> Copy s
copy = Copy

type instance IxValue (Copy s) = IxValue s
type instance Index (Copy s) = Index s

instance (Eq (Index s), Contains f s) => Contains f (Copy s) where
  contains i f c
    | i == c ^. target = underlying (contains (c ^. source) f) c
    | otherwise = underlying (contains i f) c

instance (Eq (Index s), Ixed f s) => Ixed f (Copy s) where
  ix i f c
    | i == c ^. target = underlying (ix (c ^. source) f) c
    | otherwise = underlying (ix i f) c
