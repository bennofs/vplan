{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Combinators for building schedules, using the modifiers provided by this package.
module Data.VPlan.Combinators
(   (!<-)
  , (!<|)
  , (-||-)
  , blank
  , single
  , eq
  , except
  , ref
  , move
  , swap
  , every
  , buildCombine
  ) where

import           Control.Lens                  hiding (at)
import           Data.Monoid
import           Data.VPlan.Builder
import           Data.VPlan.Modifier.Combine
import           Data.VPlan.Modifier.Constant
import qualified Data.VPlan.Modifier.Empty     as E
import           Data.VPlan.Modifier.Limit
import qualified Data.VPlan.Modifier.Reference as R
import           Data.VPlan.Modifier.Repeat
import           Data.VPlan.Schedule

-- | @a !<- b@ applies the modifier a to the modifier b by first putting modifier a in a 'Schedule', then applying
-- modifier b to that schedule and wrapping the result again in a 'Schedule.
(!<-) :: (Supported m s, Supported n q) => (q c i v -> m s c i v) -> n q c i v -> s c i v
a !<- b = new $ a $ new b

-- | @a <-| b@ is similar to @a <-! b@, with the difference that b is already a 'Schedule'.
(!<|) :: (Supported m s) => (t c i v -> m s c i v) -> t c i v -> s c i v
a !<| b = new $ a b
infixr 6 !<|

-- | Combine two simple schedules using the 'Combine' modifier. This ensures that
-- on traversal, the values of the first given schedule are traversed first.
(-||-) :: (Supported Combine s) => s c i v -> s c i v -> s c i v
s -||- t = new $ view combine [s,t]
infixl 1 -||-

-- | This is just an empty schedule.
blank :: (Supported E.Empty s) => s c i v
blank = new E.Empty

-- | A schedule that contains a single value.
single :: (Supported Constant s) => v -> s c i v
single = new . view constant

-- | A schedule that contains the same value as the given schedule at some index.
ref :: (Supported R.Reference s) => i -> s c i v -> s c i v
ref x = new . R.reference x

-- Don't use 'at' as the name, because it's already taken by lens
-- | Apply a modifier only at a given index in the schedule.
eq :: (Supported Limit s, Ord i) => i -> s c i v -> s c i v
eq w a = equal w !<| a

-- | A schedule with all elements from another schedule, except those at index x.
except :: (Supported Limit s, Supported Combine s, Ord i) => i -> s c i v -> s c i v
except x s = new (lower x s) -||- new (greater x s)

-- | Build a list of schedules to sequence with (-||-). Later items take precendence over earlier items.
buildCombine :: (Supported Combine s, Supported E.Empty s) => Builder (s c i v) () -> s c i v
buildCombine = new . Combine . runBuilder

-- | Move an item to another place. @move source target@ moves an item at index @source@ to
-- index @target@. When there is already an item at the target position, the moved item is placed behind
-- the already-existing item. The source index is set to empty.
-- 
-- Note: When there is no item at the source index, the entry at the target index won't be changed at all!
move :: (Supported Combine s, Supported Limit s, Supported E.Empty s, Supported R.Reference s, Ord i)
     => i -> i -> s c i v -> s c i v
move f t s = eq t (ref f s) -||- except f s

-- | Swap two items at given indices.
swap :: (Supported Combine s, Supported Limit s, Supported R.Reference s, Ord i)
     => i -> i -> s c i v -> s c i v
swap a b s = except b (except a s) -||- eq b (ref a s) -||- eq a (ref b s)

-- | Repeat a given schedule every n time units.
every :: (Supported Repeat s, Monoid i) => i -> s c i v -> s c i v
every n = new . Repeat (n,mempty)
