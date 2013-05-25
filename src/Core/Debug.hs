{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : $Header$
-- Description : Instances of Show and other classes for debugging purposes.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable
module Core.Debug where

import           Control.Lens
import           Core.Modifier.Combine
import           Core.Modifier.Constant
import           Core.Modifier.Empty
import           Core.Modifier.Enum
import           Core.Modifier.Limit
import           Core.Modifier.Reference
import           Core.Schedule

instance Show (IxValue s) => Show (Constant s) where
  show (Constant c) = show c

deriving instance Show (Empty s)
instance (Show s, Show (Index s)) => Show (Reference s) where
  show (Reference i u) = "REF " ++ show i ++ " | " ++ show u

instance (Show (Index s), Show s) => Show (Limit s) where
  show (Limit y w s) = show y ++ " "++ show w ++ " | " ++ show s

instance (Show (l s), Show (r s)) => Show ((l :><: r) s) where
  showsPrec i (L x) = ("L " ++) . showsPrec i x
  showsPrec i (R x) = ("R " ++) . showsPrec i x

instance (Show s) => Show (Combine s) where
  show (Combine a b) = "(" ++ show a ++ " -||- " ++ show b ++ ")"

instance (Show (s (Schedule i v s))) => Show (Schedule i v s) where
  showsPrec i (Schedule x) = showsPrec i x

deriving instance Show (Close s)

data FakeIndexed i v
type instance Index (FakeIndexed i v) = i
type instance IxValue (FakeIndexed i v) = v
