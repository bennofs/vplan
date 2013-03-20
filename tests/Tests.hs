{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
import           Control.Lens
import           Core.Modifier
import           Core.Modifier.Constant
import           Core.Schedule
import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Function
import Test.QuickCheck.Classes
import qualified Test.QuickCheck.Checkers as QC

deriving instance (Arbitrary i) => Arbitrary (Constant i e)
deriving instance (Arbitrary (e (Schedule e))) => Arbitrary (Schedule e)
deriving instance (Arbitrary a) => Arbitrary (Last (Maybe a))
deriving instance (Eq i) => Eq (Constant i e)
deriving instance (QC.EqProp i) => QC.EqProp (Constant i e)

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
  testProperty "alternative2" prop_alternative2,
  testProperty "Constant modifier single" prop_constant,
  testBatch "Constant functor" $ functor (undefined :: Constant Int (Int, Float, Bool)),
  testBatch "Constant applicative" $ applicative (undefined :: Constant [Int] (Int, Float, Bool))
--  testProperty "Constant modifier sequential" prop_constant_seq
  ]

prop_alternative2 :: Fun Int (Fun Int Int) -> Int -> Int -> Bool
prop_alternative2 (Fun _ f) a b = p1 && p2 && p3
  where p1 = alternative2 (apply . f) (Just a) (Just b) == Just (apply (f a) b)
        p2 = alternative2 (apply . f) Nothing (Just b) == Just b
        p3 = alternative2 (apply . f) (Just a) Nothing == Just a

prop_constant :: Last (Maybe Int) -> Int -> Bool
prop_constant x i = runSchedule (x ^. constant . schedule) i == x
