{-# LANGUAGE BangPatterns #-}
-- | Miscellaneous functions and types that belong to no other module
module Data.VPlan.Util
  (
   -- * Group and Monoid functions
   -- Note: The functions for monoids/groups in this module are not really efficient, but
   -- that shouldn't matter most of the time.
    gquot
  , gmod
  , gquotMod
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
-- a 'quot' function that works on arbitrary groups.
--
-- Examples:
--
-- >>> (Sum 11) `gmod` (Sum 2) (Sum 11)
-- 5
--
-- >>> (Product 27) `gmod` (Product 3)
-- 3
gquot :: (Ord a, Group a) => a -> a -> Int
gquot xs x = fst $ xs `gquotMod` x

-- | A version of divMod that works for arbitrary groups.
gquotMod :: (Ord a, Group a) => a -> a -> (Int, a)
gquotMod xs x
  | xs < mempty = over _1 negate $ over _2 (\r -> if r /= mempty then x <> invert r else r) $ gquotMod (invert xs) x
  | x < mempty = over _1 negate $ over _2 (mappend x) $ gquotMod xs (invert x)
  | x > xs = (0,xs)
  | x == xs = (1,mempty)
  | otherwise = over _1 (+ getSum steps) $ (xs <> invert half) `gquotMod` x
  where (steps, half) = until moreThanHalf (join mappend) (Sum 1, x)
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
gmod xs x = snd $ xs `gquotMod` x

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
                    
