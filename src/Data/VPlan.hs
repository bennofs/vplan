{-# LANGUAGE TypeOperators #-}

-- | A module that reexports most of the libraries modules.
module Data.VPlan
 ( -- * Exported modules
   -- ** Schedule data type and instances for it
   module Data.VPlan.Schedule

   -- ** Simple functions for creating schedules.
 , module Data.VPlan.Combinators
 , module Data.VPlan.Class

   -- ** Functions for modifying schedules
   -- Nearly all functions for indexing schedules are provided by lens
 , module Control.Lens

   -- ** Functions for printing schedule
 , module Data.VPlan.Print

   -- ** Modifiers
   -- These are sorted after complexity, so you can look at the easier ones as an example for implementing modifiers.
 , module Data.VPlan.Modifier.Empty
 , module Data.VPlan.Modifier.Constant
 , module Data.VPlan.Modifier.Annotate
 , module Data.VPlan.Modifier.Reference
 , module Data.VPlan.Modifier.Repeat
 , module Data.VPlan.Modifier.Limit
 , module Data.VPlan.Modifier.Combine
 , module Data.VPlan.Modifier.Enum

   -- ** Defining new modifiers
 , module Data.VPlan.TH

   -- * Convenience definitions
 , AllModifiers
 , USchedule

 ) where

import           Control.Lens
import           Data.VPlan.Class
import           Data.VPlan.Combinators
import           Data.VPlan.Modifier.Annotate
import           Data.VPlan.Modifier.Combine
import           Data.VPlan.Modifier.Constant
import           Data.VPlan.Modifier.Empty
import           Data.VPlan.Modifier.Enum
import           Data.VPlan.Modifier.Limit
import           Data.VPlan.Modifier.Reference
import           Data.VPlan.Modifier.Repeat
import           Data.VPlan.Print
import           Data.VPlan.Schedule
import           Data.VPlan.TH

-- | A type alias for all the available modifiers, to avoid having to write out those everytime.
type AllModifiers = Constant :><: Combine :><: Empty :><: Limit :><: Reference

-- | An universal schedule, that can supports all the modifier provided in this package.
type USchedule i v = Schedule AllModifiers i v
