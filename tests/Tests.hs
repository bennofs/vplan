{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
import           Control.Lens                         hiding (at)
import           Core.Modifier.Constant
import Core.Modifier.Empty
import           Core.Schedule
import           Core.SimpleSchedule
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import qualified Test.QuickCheck.Checkers             as QC

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "Schedule and Modifiers" scheduleModifiers
  ]

testBatch :: TestName -> QC.TestBatch -> Test
testBatch n = testGroup n . map (uncurry testProperty) . QC.unbatch

scheduleModifiers :: [Test]
scheduleModifiers =
  [ testProperty "Empty schedule contains" prop_empty_contains
--  , testProperty "Empty schedule ix" prop_empty_ix
--  , testProperty "Single schedule contains" prop_single_contains
--  , testProperty "Single schedule ix" prop_single_ix
--  , testProperty "at contains" prop_at_contains
--  , testProperty "at ix" prop_at_ix
--  , testProperty "schedule iso" prop_schedule_iso
--  , testProperty "schedule Eq" prop_eq_schedule
--  , testProperty "empty Eq" $ Empty == Empty
  ]

prop_empty_contains :: Int -> Bool
prop_empty_contains x = (empty :: SimpleSchedule Int Int) ^. contains x == False

prop_empty_ix :: Int -> Bool
prop_empty_ix x = (empty :: SimpleSchedule Int Int) ^.. ix x == []

prop_single_contains :: Int -> Int -> Bool
prop_single_contains x y = (single x :: SimpleSchedule Int Int) ^. contains y == True

prop_single_ix :: Int -> Int -> Bool
prop_single_ix x y = (single x :: SimpleSchedule Int Int) ^@.. ix y == [(y,x)]

prop_schedule_iso :: Int -> Bool
prop_schedule_iso x = view schedule (review schedule s) == s
                      && review schedule (view schedule s2) == s2
  where s2 = x ^. constant :: Constant (Schedule Int Int Constant)
        s = single x :: SimpleSchedule Int Int

prop_at_contains :: Int -> Int -> Bool
prop_at_contains x y = s ^. contains x == True
                       && s ^. contains (succ x) == False
                       && s ^. contains (pred x) == False
  where s = at x (single y)

prop_at_ix :: Int -> Int -> Bool
prop_at_ix x y = s ^.. ix x == [y] && s ^.. ix (pred x) == [] && s ^.. ix (succ x) == []
  where s = at x (single y)

prop_eq_schedule :: Int -> Int -> Int -> Int -> Bool
prop_eq_schedule w x y z = s == s where
  s = move w y $ at w (single x) -||- at y (single z) -||- at z empty
