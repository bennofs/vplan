{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Test for the combinators in the Data.VPlan.Combinators module
module Combinators where

import           Control.Lens
import           Data.VPlan
import           Instances             ()
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

type Timetable = USchedule DiscreteTime Int

neither :: (Eq a) => [a] -> a -> Bool
neither of' = not . (`elem` of')

forIndicesEqual :: Gen DiscreteTime -> Timetable -> Timetable -> Property
forIndicesEqual g a b = forAll g $ \i -> a ^? ix i == b ^? ix i

forIndicesEmpty :: Gen DiscreteTime -> Timetable -> Property
forIndicesEmpty g a = forAll g $ \i -> not $ a ^. contains i

prop_blank :: Property
prop_blank = forIndicesEmpty arbitrary blank

prop_single :: DiscreteTime -> Int -> Bool
prop_single i v = (single v :: Timetable) ^? ix i == Just v

prop_ref :: DiscreteTime -> Timetable -> Property
prop_ref i s = forAll arbitrary $ \t -> sch ^? ix t == s ^? ix i
  where sch = ref i s

prop_eq :: DiscreteTime -> Timetable -> Property
prop_eq i s = sch ^? ix i == s ^? ix i .&. forIndicesEmpty (arbitrary `suchThat` (/= i)) sch
  where sch = eq i s

prop_except :: DiscreteTime -> Timetable -> Property
prop_except i s  = not (sch ^. contains i) .&. forIndicesEqual (arbitrary `suchThat` (/= i)) sch s
  where sch = except i s

prop_move :: DiscreteTime -> DiscreteTime -> Timetable -> Property
prop_move f t s = not (sch ^. contains f) .&. sch ^.. ix t == (s ^.. ix f ++ s ^.. ix t) .&. forIndicesEqual (arbitrary `suchThat` neither [f,t]) sch s
  where sch = move f t s
        prop
          | t == f = sch ^.. ix t == s ^.. ix f
          | otherwise = sch ^.. ix t == s ^.. ix f ++ s ^.. ix t

prop_swap :: DiscreteTime -> DiscreteTime -> Timetable -> Property
prop_swap i1 i2 s = sch ^? ix i1 == s ^? ix i2 .&. sch ^? ix i2 == s ^? ix i1 .&. forIndicesEqual (arbitrary `suchThat` neither [i1,i2]) sch s
  where sch = swap i1 i2 s

prop_repeat :: DiscreteTime -> Timetable -> Property
prop_repeat i s = forAll arbitrary $ \b -> all (\o -> sch ^? ix (b * i + o) == s ^? ix o) [0..pred i]
  where sch = every i s

properties :: TestTree
properties = $(testGroupGenerator)
