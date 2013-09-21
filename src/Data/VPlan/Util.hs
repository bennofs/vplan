{-# LANGUAGE BangPatterns #-}
-- | Miscellaneous functions and types that belong to no other module
module Data.VPlan.Util
  (
   -- * Group and Monoid functions
   -- Note: The functions for monoids/groups in this module are not really efficient, but
   -- that shouldn't matter most of the time.
    gdiv
  , gmod
  , gdivMod
  , glcm
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Group
import           Data.Monoid

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
gdiv n d = fst $ n `gdivMod` d

-- | A version of divMod that works for arbitrary groups.
gdivMod :: (Ord a, Group a) => a -> a -> (Int, a)
gdivMod n d
  | n < mempty = over _1 negate $ over _2 (\r -> d <> invert r) $ gdivMod (invert n) d
  | d < mempty = over _1 negate $ over _2 (mappend d) $ gdivMod n (invert d)
  | d > n = (0,n)
  | d == n = (1,mempty)
  | otherwise = over _1 (+ getSum steps) $ (n <> invert half) `gdivMod` d
  where (steps, half) = until moreThanHalf (join mappend) (Sum 1, d)
        moreThanHalf (_,a) = (n <> invert a) < a

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
gmod n d = snd $ n `gdivMod` d

-- | This is like lcm, but for arbitrary monoids.
--
-- Examples:
--
-- >>> (Sum 6) `glcm` (Sum 4)
-- Sum 12
--
-- >>> (Product 4) `glcm` (Product 8)
-- Product 64 -- = Product (4 * 4 * 4) = Product (8 * 8)
--
glcm :: (Ord a, Monoid a) => a -> a -> a
glcm a' b' = go a' b'
  where go !a !b
          | a > b = go a (b <> b')
          | a < b = go (a <> a') b
          | otherwise = a
                    
