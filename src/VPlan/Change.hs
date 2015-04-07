{-# LANGUAGE MultiParamTypeClasses #-}
module Data.VPlan.Change where

import Control.Lens hiding (Action, act)
import Data.Group
import Data.Monoid.Action
import Data.VPlan.Combinators
import Data.VPlan.Plan

data Change i v = Move i i
                | NewValue i v
                | Delete i
                | Other i String

instance (Group i, Ord i) => Action (Change i v) (Plan i v) where
  act (Move f t) = move f t
  act (NewValue i v) = set (ix i) v
  act (Delete i) = delete i
  act (Other _ _) = id
