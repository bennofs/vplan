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
module Core.Modifier.Annotate
  ( Annotate(..)
  , attached
  , underlying
  ) where

import           Control.Lens
import qualified Core.AtSansFunctor as A
import           Core.TH
import           Data.Data

-- | @Annotate a@ is a modifier that can attach data of type @a@ to some other modifier.
data Annotate a s = Annotate
  { _attached   :: a       -- ^ Contains the attached value
  , _underlying :: s       -- ^ Contains the modifier the value is attached to
  } deriving (Eq)

makeLenses ''Annotate
makeModifier ''Annotate

deriving instance (Data a, Data s) => Data (Annotate a s)

instance (A.Contains f s, Functor f) => A.Contains f (Annotate a s) where
  contains = fmap underlying . A.contains

instance (A.Ixed f s, Functor f) => A.Ixed f (Annotate a s) where
  ix = fmap underlying . A.ix
