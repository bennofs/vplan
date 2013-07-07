{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | A modifier that references a value in a schedule
module Data.VPlan.Modifier.Reference (
    Reference(..)
  , source
  , reference
  ) where

import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At    as A
import           Data.VPlan.Class
import           Data.VPlan.TH

-- | Reference the value at index '_source' in the schedule '_underlying'.
data Reference s = Reference {_source :: Index s, _underlying :: s}
makeLenses ''Reference
makeModifier ''Reference
deriveClass ''Reference

deriving instance (Eq (Index s), Eq s) => Eq (Reference s)
deriving instance (Data (Index s), Data s) => Data (Reference s)

-- | Construct a new reference.
reference :: Index s -> s -> Reference s
reference = Reference

instance (Eq (Index s), Gettable f, A.Contains f s) => A.Contains f (Reference s) where
  contains _ f r = underlying ?? r $ A.contains (r ^. source) f

instance (Eq (Index s), A.Ixed f s, Functor f) => A.Ixed f (Reference s) where
  ix _ f r = underlying ?? r $ A.ix (r ^. source) f
