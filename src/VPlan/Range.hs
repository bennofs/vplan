{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Data.VPlan.Range where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Group
import Data.Monoid

data Range i = Range { _start :: i, _end :: i } deriving (Eq, Ord)
makeLenses ''Range

type instance Index (Range i) = i

instance Ord i => Contains (Range i) where
  contains = containsTest $ \i (Range a b) -> i >= a && i < b

instance Functor Range where
  fmap f (Range a b) = Range (f a) (f b)

instance Applicative Range where
  pure = join Range
  Range f g <*> Range a b = Range (f a) (g b)

size :: Group i => Range i -> i
size r = r ^. end <> r ^. start . to invert

before :: Ord i => i -> Range i -> Bool
before i (Range a _) = i < a

after :: Ord i => i -> Range i -> Bool
after i (Range _ b) = i >= b

overlapping :: Ord i => Range i -> Range i -> Bool
overlapping a b
  | a ^. start > b ^. start = overlapping b a
  | otherwise = a ^. end > b ^. start

merge :: Ord i => Range i -> Range i -> Range i
merge a b = Range min max <*> a <*> b

