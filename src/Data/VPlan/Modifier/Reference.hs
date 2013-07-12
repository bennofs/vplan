{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
data Reference s i v = Reference {_source :: i, _underlying :: s i v} deriving (Eq)
makeLenses ''Reference
makeModifier ''Reference
deriveClass ''Reference

deriving instance (Data i, Data (s i v), Typeable2 s, Typeable i, Typeable v) => Data (Reference s i v)

-- | Construct a new reference.
reference :: i -> s i v -> Reference s i v
reference = Reference

instance (Eq i, i ~ Index (s i v), Gettable f, A.Contains f (s i v)) => A.Contains f (Reference s i v) where
  contains _ f r = underlying ?? r $ A.contains (r ^. source) f

instance (Eq i, i ~ Index (s i v), A.Ixed f (s i v), Functor f) => A.Ixed f (Reference s i v) where
  ix _ f r = underlying ?? r $ A.ix (r ^. source) f
