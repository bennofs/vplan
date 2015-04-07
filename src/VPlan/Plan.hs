{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
module Data.VPlan.Plan where

import Control.Applicative
import Control.Lens hiding (Action, act)
import Data.Group
import Data.Monoid
import Data.Monoid.Action
import Data.Monoid.Coproduct
import Data.Semigroup (Semigroup)
import Data.VPlan.Range
import Data.VPlan.Util

newtype Every i = Every (Product i) deriving (Semigroup, Monoid)
newtype Exclude i = Exclude { getExclude :: [Range i] }

instance (Ord i, Group i) => Monoid (Exclude i) where
  mempty = Exclude []
  mappend (Exclude xs) (Exclude ys) = Exclude $ mergeAdjacent $ go xs ys
    where mergeAdjacent (a:b:as)
            | overlapping a b = mergeAdjacent $ merge a b : as
            | otherwise = a : mergeAdjacent (b : as)
          mergeAdjacent x = x
          go (a:as) (b:bs)
            | a < b = a : go as (b:bs)
            | otherwise = b : go (a:as) bs
          go [] bs = bs
          go as [] = as

instance (Ord i, Group i) => Action (Every i) (Maybe i) where
  act (Every x) = fmap (`gmod` getProduct x)

instance (Ord i, Group i) => Action (Exclude i) (Maybe i) where
  act (Exclude xs) a = do
    i <- a
    let go [] !r = Just (i <> invert r)
        go (r : as) s
          | r ^. contains i = Nothing
          | i `before` r = go [] s
          | otherwise = go as (s <> size r)
    go xs mempty

newtype Transformation i = Transformation (Exclude i :+: Every i) deriving (Semigroup, Monoid)
instance (Group i, Ord i) => Action (Transformation i) (Maybe i) where
  act (Transformation x) = act x

-- | A matcher is a simple query expression that returns either 'True' or 'False' for a given input.
data Matcher i = Or (Matcher i) (Matcher i)
               | And (Matcher i) (Matcher i)
               | Not (Matcher i)
               | When Ordering i (Matcher i)
               | Always Bool deriving (Show)

type instance Index (Matcher i) = i

instance (Group i, Ord i) => Contains (Matcher i) where
  contains _ f (Always r) = coerce $ f r
  contains i f (Or a b) = coerce $ f $ a ^. contains i || b ^. contains i
  contains i f (And a b) = coerce $ f $ a ^. contains i && b ^. contains i
  contains i f (Not a) = coerce $ f $ not $ a ^. contains i
  contains i f (When cond x a) = coerce $ f $ compare i x == cond &&  a ^. contains i

data Entry i v = Entry
  { _value :: v
  , _matcher :: Matcher i
  } deriving (Show)
makeLenses ''Entry
  
type instance Index (Entry i v) = i
type instance IxValue (Entry i v) = v

instance (Group i, Ord i) => Contains (Entry i v) where
  contains i = matcher . contains i
  
instance (Group i, Ord i) => Ixed (Entry i v) where
  ix i f (Entry v m)
    | m ^. contains i = flip Entry m <$> f v
    | otherwise = pure $ Entry v m

data Plan i v = Plan [Entry i v]
              | Transformed (Transformation i) (Plan i v)
              | Combined [Plan i v]

type instance Index (Plan i v) = i
type instance IxValue (Plan i v) = v

instance (Group i, Ord i) => Contains (Plan i v) where contains = containsIx
instance (Group i, Ord i) => Ixed (Plan i v) where
  ix i f (Plan es) = Plan <$> traverse (ix i f) es
  ix i f (Transformed t p) = Transformed t <$> do maybe (pure p) (\i' -> ix i' f p) $ act t $ Just i
  ix i f (Combined ps) = Combined <$> traverse (ix i f) ps

instance Functor (Entry i) where fmap = over value
instance Functor (Plan i) where
  fmap f (Plan es) = Plan $ fmap f <$> es
  fmap f (Transformed t p) = Transformed t $ fmap f p
  fmap f (Combined ps) = Combined $ fmap f <$> ps

instance Monoid (Plan i v) where
  mempty = Plan []
  Plan a `mappend` Plan b = Plan $ a <> b
  Combined as `mappend` Combined ps = Combined $ as <> ps
  a `mappend` Combined ps = Combined $ a : ps
  Combined ps `mappend` a = Combined $ a : ps
  a `mappend` b = Combined [a,b]

instance Action (Transformation i) (Plan i v) where
  act t (Transformed t' p) = Transformed (t <> t') p
  act t x = Transformed t x

entriesAt :: (Group i, Ord i) => i -> Traversal' (Plan i v) (Entry i v)
entriesAt i f (Plan es) = Plan <$> (traverse . filtered (view $ matcher . contains i)) f es
entriesAt i f (Transformed t p) = Transformed t <$> entriesAt i f p
entriesAt i f (Combined ps) = Combined <$> traverse (entriesAt i f) ps

