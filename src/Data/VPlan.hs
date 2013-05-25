-- |
-- Module      : $Header$
-- Description : Main module that reexports the simpler schedule combinators.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (uses various GHC extensions)

module Data.VPlan
 ( -- * Contains the Schedule data type and instances for it
   module Schedule

   -- * This module contains some simple functions for creating schedules.
 , module Combinators

   -- * Much of the functionality in this package is implemented in terms of lens, so export it here.
 , module Lens

   -- * Some modifiers that you might need (sorted after complexity, so you can look at these as examples)
 , module Empty
 , module Constant
 , module Reference
 , module Annotate
 , module Limit
 , module Combine
 , module Enum

 ) where

import           Control.Lens                  as Lens
import           Data.VPlan.Combinators        as Combinators
import           Data.VPlan.Modifier.Annotate  as Annotate
import           Data.VPlan.Modifier.Combine   as Combine
import           Data.VPlan.Modifier.Constant  as Constant
import           Data.VPlan.Modifier.Empty     as Empty
import           Data.VPlan.Modifier.Enum      as Enum
import           Data.VPlan.Modifier.Limit     as Limit
import           Data.VPlan.Modifier.Reference as Reference
import           Data.VPlan.Schedule           as Schedule
