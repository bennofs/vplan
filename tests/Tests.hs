{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
import           Control.Lens                         hiding (at)
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
scheduleModifiers = [
    testProperty "Empty is empty" prop_empty_sched
  , testProperty "Create schedule" prop_create
  , testProperty "Insert schedule" prop_insert
  ]

prop_empty_sched :: Int -> Bool
prop_empty_sched x = (delete :: SimpleSchedule Int Int) ^.. ix x == []

prop_create :: Int -> Int -> Bool
prop_create x y = (create x :: SimpleSchedule Int Int) ^.. ix y == [x]

prop_insert :: Int -> Int -> Bool
prop_insert x y = ins ^.. ix x == [y] && ins ^.. ix (pred x) == [] && ins ^.. ix (succ x) == []
  where ins = at x (create y)
