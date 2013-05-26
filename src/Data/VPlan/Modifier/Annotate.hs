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
-- Description : A modifier that allows to attach some additional static data to another modifier.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable
module Data.VPlan.Modifier.Annotate
  ( Annotate(..)
  , annotate
  , attached
  , annotated
  ) where

import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At    as A
import           Data.VPlan.Class
import           Data.VPlan.TH

-- | @Annotate a@ is a modifier that can attach data of type @a@ to some other modifier.
data Annotate a s = Annotate
  { _attached  :: a       -- ^ Contains the attached value
  , _annotated :: s        -- ^ Contains the modifier the value is attached to
  } deriving (Eq)

makeLenses ''Annotate
makeModifier ''Annotate
deriveClass ''Annotate

-- | Annotate another modifier with a value of type @a@.
annotate :: a -> s -> Annotate a s
annotate = Annotate

deriving instance (Data a, Data s) => Data (Annotate a s)

instance (A.Contains f s, Functor f) => A.Contains f (Annotate a s) where
  contains = fmap annotated . A.contains

instance (A.Ixed f s, Functor f) => A.Ixed f (Annotate a s) where
  ix = fmap annotated . A.ix
