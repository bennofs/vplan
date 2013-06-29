{-# LANGUAGE TypeOperators #-}
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
 , module Class

   -- * Much of the functionality in this package is implemented in terms of lens, so export it here.
 , module Lens

   -- * Some modifiers that you might need (sorted after complexity, so you can look at these as examples)
 , module Empty
 , module Constant
 , module Annotate
 , module Reference
 , module Repeat
 , module Limit
 , module Combine
 , module Enum

   -- * Template haskell for defining new modifiers
 , module TH

   -- * Convenience definitions
 , AllModifiers
 , USchedule

 ) where

import           Control.Lens                  as Lens
import           Data.VPlan.Class              as Class
import           Data.VPlan.Combinators        as Combinators
import           Data.VPlan.Modifier.Annotate  as Annotate
import           Data.VPlan.Modifier.Combine   as Combine
import           Data.VPlan.Modifier.Constant  as Constant
import           Data.VPlan.Modifier.Empty     as Empty
import           Data.VPlan.Modifier.Enum      as Enum
import           Data.VPlan.Modifier.Limit     as Limit
import           Data.VPlan.Modifier.Reference as Reference
import           Data.VPlan.Modifier.Repeat    as Repeat
import           Data.VPlan.Schedule           as Schedule
import           Data.VPlan.TH                 as TH

-- | A type alias for all the available modifiers, to avoid having to write out those everytime.
type AllModifiers = Constant :><: Combine :><: Empty :><: Limit :><: Reference :><: Repeat

-- | An universal schedule, that can supports all the modifier provided in this package.
type USchedule i v = Schedule i v AllModifiers
