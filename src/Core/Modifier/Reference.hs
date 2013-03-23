{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : $Header$
-- Description : A modifier that references a value in a schedule
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (Uses various GHC extensions)
module Core.Modifier.Reference (
    Reference(..)
  , source
  , reference
  ) where

import           Control.Lens

-- | Reference the value at index '_source' in the schedule '_underlying'.
data Reference s = Reference {_source :: Index s, _underlying :: s}
makeLenses ''Reference

deriving instance (Eq (Index s), Eq s) => Eq (Reference s)

-- | Construct a new reference.
reference :: Index s -> s -> Reference s
reference = Reference

type instance IxValue (Reference s) = IxValue s
type instance Index (Reference s) = Index s

instance (Eq (Index s), Gettable f, Contains f s) => Contains f (Reference s) where
  contains _ f r = underlying ?? r $ contains (r ^. source) f

instance (Eq (Index s), Ixed f s) => Ixed f (Reference s) where
  ix _ f r = underlying ?? r $ ix (r ^. source) f
