{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
import           Control.Lens                         hiding (at)
import           Data.Maybe
import           Data.VPlan
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Schedule and Modifiers" scheduleModifiers
  ]

scheduleModifiers :: [Test]
scheduleModifiers =
  [ testProperty "Empty schedule contains" prop_empty_contains
  , testProperty "Empty schedule ix" prop_empty_ix
  , testProperty "Single schedule contains" prop_single_contains
  , testProperty "Single schedule ix" prop_single_ix
  , testProperty "at contains" prop_at_contains
  , testProperty "at ix" prop_at_ix
  , testProperty "schedule iso" prop_schedule_iso
  , testProperty "schedule Eq" prop_eq_schedule
  , testProperty "empty Eq" $ Empty == Empty
  , testProperty "move removes old sets new" prop_move
  , testProperty "swap swaps two items" prop_swap
  ]

type SimpleSchedule a b = Schedule a b (Constant :><: Limit :><: Empty :><: Combine :><: Reference)

prop_empty_contains :: Int -> Bool
prop_empty_contains x = not $ (empty :: SimpleSchedule Int Int) ^. contains x

prop_empty_ix :: Int -> Bool
prop_empty_ix x = null $ (empty :: SimpleSchedule Int Int) ^.. ix x

prop_single_contains :: Int -> Int -> Bool
prop_single_contains x y = (single x :: SimpleSchedule Int Int) ^. contains y

prop_single_ix :: Int -> Int -> Bool
prop_single_ix x y = (single x :: SimpleSchedule Int Int) ^@.. ix y == [(y,x)]

prop_schedule_iso :: Int -> Bool
prop_schedule_iso x = view schedule (review schedule s) == s
                      && review schedule (view schedule s2) == s2
  where s2 = x ^. constant :: Constant (Schedule Int Int Constant)
        s = single x :: SimpleSchedule Int Int

prop_at_contains :: Int -> Int -> Bool
prop_at_contains x y = s ^. contains x
                       && not (s ^. contains (succ x))
                       && not (s ^. contains (pred x))
  where s = eq x (single y) :: SimpleSchedule Int Int

prop_at_ix :: Int -> Int -> Bool
prop_at_ix x y = s ^.. ix x == [y] && null (s ^.. ix (pred x)) && null (s ^.. ix (succ x))
  where s = eq x (single y) :: SimpleSchedule Int Int

prop_eq_schedule :: Int -> Int -> Int -> Int -> Bool
prop_eq_schedule w x y z = s == s where
  s = move w y $ eq w (single x) -||- eq y (single z) -||- eq z empty :: SimpleSchedule Int Int

prop_move :: Int -> Int -> Int -> Bool
prop_move i f t
  | f /= t && f /= i && i /= t = s ^? ix t == Just i && isNothing (s ^? ix f) && s ^? ix i == Just (i + 3)
  | f == t = isNothing $ s ^? ix t
  | otherwise = s ^? ix t == Just i
  where s = (move f t $ except t $ eq f (single i) -||- eq i (single $ i+3)) :: SimpleSchedule Int Int

prop_swap :: Int -> Int -> Int -> Int -> Bool
prop_swap x y a b
  | a /= b && a /= x && b /= x = s ^? ix a == Just y && s ^? ix b == Just x && s ^? ix x == Just (a + 3)
  | a == b = s ^? ix a == Just x
  | otherwise = s ^? ix a == Just y && s ^? ix b == Just x
  where s = swap a b $ eq a (single x) -||- eq b (single y) -||- eq x (single $ a+3)  :: SimpleSchedule Int Int
