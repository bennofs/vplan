-- |
-- Module      : $Header$
-- Description : Utilities for using the Writer monad to build a list of things.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : portable

module Data.VPlan.Builder
 ( -- * Data type definition
   Builder
 , runBuilder
   -- * Building items
 , item
 , list
 , dlist
   -- * Operations on Builders
 , mapBuilder
 ) where

import           Control.Monad.Writer
import           Data.DList           (DList)
import qualified Data.DList           as DL

-- | A Builder is just a writer with a difference list as accumulator for the built items.
-- The ordering of the items will be preserved, i.e. items build first will appear first
-- in the resulting list.
type Builder b a = Writer (DList b) a

-- | Run a builder and return the items. The ordering of the items will be preserved.
runBuilder :: Builder b () -> [b]
runBuilder b = DL.toList $ execWriter b

-- | Add a single item to a 'Builder'.
item :: a -> Builder a ()
item = dlist . DL.singleton

-- | Add items in a DList to a builder. This is just tell.
dlist :: DList a -> Builder a ()
dlist = tell

-- | Add all items in a list to builder.
list :: [a] -> Builder a ()
list = dlist . DL.fromList

-- | Map a function over the output of a builder.
mapBuilder :: (a -> a) -> Builder a b -> Builder a b
mapBuilder f = censor (DL.map f)
