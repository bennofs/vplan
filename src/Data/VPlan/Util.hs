{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Miscellaneous functions and types that belong to no other module
module Data.VPlan.Util
  (
   -- * Functions
    gdiv
  , gmod
  , gdivMod
   -- * Types
  , (:$)
  , Flip
  , By
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Group
import           Data.Monoid

-- | This is '($)' at the type level.
type a :$ b = a b
infixr 0 :$

-- | This is 'flip' at the type level.
type Flip a b c = a c b

-- | Alias for 'Flip'.
type By a b c = a c b

-- TODO: Maybe a better name for the following 3 functions?
-- Suggestions:  gdiv:    howMany
--               gdivMod: ???
--               gmod:    rest?, ???

-- | This is the inverse of 'timesN'. It calculates how many times a given
-- group object fits into another object of the same group. It's basically
-- a 'div' function that works on arbitrary groups.
--
-- Examples:
--
-- >>> (Sum 11) `gmod` (Sum 2) (Sum 11)
-- 5
--
-- >>> (Product 27) `gmod` (Product 3)
-- 3
gdiv :: (Ord a, Group a) => a -> a -> Int
gdiv xs x = fst $ xs `gdivMod` x

-- | A version of divMod that works for arbitrary groups.
gdivMod :: (Ord a, Group a) => a -> a -> (Int, a)
gdivMod xs x
  | xs < mempty = over _1 negate $ over _2 (mappend x . invert) $ gdivMod (invert xs) x
  | x < mempty = over _1 negate $ over _2 (mappend x) $ gdivMod xs (invert x)
  | x > xs = (0,xs)
  | x == xs = (1,mempty)
  | otherwise = over _1 (+ getSum steps) $ (xs <> invert half) `gdivMod` x
  where (steps, half) = until moreThanHalf (join mappend) $ (Sum 1, x)
        moreThanHalf (_,a) = (xs <> invert a) < a

-- | This is like mod, but for arbitrary groups.
--
-- Examples:
--
-- >>> (Sum 11) `gmod` (Sum 2)
-- Sum 1
--
-- >>> (Product 29) `gmod` (Product $ 3 % 1)
-- Product {getProduct = 29 % 27}
gmod :: (Ord a, Group a) => a -> a -> a
gmod xs x= snd $ xs `gdivMod` x
