{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : $Header$
-- Description : Combinators for building schedules, using the modifiers provided by this package.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable
module Data.VPlan.Combinators
(   (!<-)
  , (!<|)
  , (-||-)
  , empty
  , single
  , eq
  , ref
  , move
  , swap
  , buildCombine
  ) where

import           Control.Lens                  hiding (at)
import           Data.VPlan.Builder
import           Data.VPlan.Modifier.Combine
import           Data.VPlan.Modifier.Constant
import qualified Data.VPlan.Modifier.Empty     as E
import           Data.VPlan.Modifier.Limit
import qualified Data.VPlan.Modifier.Reference as R
import           Data.VPlan.Schedule

-- | @a !<- b@ applies the modifier a to the modifier b by first putting modifier a in a 'Schedule', then applying
-- modifier b to that schedule and wrapping the result again in a 'Schedule.
(!<-) :: (Supported m s, Supported n q) => (q -> m s) -> n q -> s
a !<- b = new $ a $ new b

-- | @a <-| b@ is similar to @a <-! b@, with the difference that b is already a 'Schedule'.
(!<|) :: (Supported m s) => (t -> m s) -> t -> s
a !<| b = new $ a b

-- | Combine two simple schedules using the 'Combine' modifier. This ensures that
-- on traversal, the values of the first given schedule are traversed first.
(-||-) :: (Supported Combine s) => s -> s -> s
s -||- t = new $ combine s t

-- | This is just an empty schedule.
empty :: (Supported E.Empty s) => s
empty = new E.Empty

-- | A schedule that contains a single value.
single :: (Supported Constant s) => IxValue s -> s
single = new . view constant

-- | A schedule that contains the same value as the given schedule at some index.
ref :: (Supported R.Reference s) => Index s -> s -> s
ref x = new . R.reference x

-- Don't use at as the name, because it's already taken by lens
-- | Apply a modifier only at a given index in the schedule.
eq :: (Supported Limit s) => Index s -> s -> s
eq w a = equal w !<| a

-- | Build a list of schedules to sequence with (-||-). Later items take precendence over earlier items.
buildCombine :: (Supported Combine s, Supported E.Empty s) => Builder s () -> s
buildCombine = foldr (-||-) empty . runBuilder

-- | Move an item to another place. @move source target@ moves an item at index @source@ to
-- index @target@.
move :: (Supported Combine s, Supported Limit s, Supported E.Empty s, Supported R.Reference s)
     => Index s -> Index s -> s -> s
move f t s = eq f empty -||- eq t (ref f s)

-- | Swap two items at given indices.
swap :: (Supported Combine s, Supported Limit s, Supported R.Reference s)
     => Index s -> Index s -> s -> s
swap a b s = eq b (ref a s) -||- eq a (ref b s)