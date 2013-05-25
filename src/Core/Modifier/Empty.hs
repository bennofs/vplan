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
-- Description : A modifier that contains no value.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable

module Core.Modifier.Empty (
    Empty(..)
  ) where

import           Control.Applicative hiding (empty)
import           Control.Lens
import qualified Core.AtSansFunctor  as A
import           Core.TH
import           Data.Data

data Empty s = Empty deriving (Eq)
makeModifier ''Empty

deriving instance (Data s) => Data (Empty s)
instance (Gettable f) => A.Contains f (Empty s) where contains = containsTest $ const $ const False
instance (Applicative f) => A.Ixed f (Empty s) where ix _ _ = pure
