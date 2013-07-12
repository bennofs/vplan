{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | A modifier that contains no value.
module Data.VPlan.Modifier.Empty (
    Empty(..)
  ) where

import           Control.Applicative hiding (empty)
import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At       as A
import           Data.VPlan.Class
import           Data.VPlan.TH

-- | This doesn't contain any value, it just ignores the s parameter (except for the IxValue/Ixed families)
data Empty (s :: * -> * -> *) i v = Empty deriving (Eq)
makeModifier ''Empty
deriveClass ''Empty

deriving instance (Typeable2 s, Typeable i, Typeable v) => Data (Empty s i v)
instance (Gettable f) => A.Contains f (Empty s i v) where contains = containsTest $ const $ const False
instance (Applicative f) => A.Ixed f (Empty s i v) where ix _ _ = pure
