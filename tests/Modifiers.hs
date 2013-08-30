{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}

-- | Test the modifier instances and behaviours
module Modifiers where

import           Control.Lens
import           Data.Aeson
import           Data.Maybe
import           Data.Semigroup
import           Data.VPlan
import           Instances                ()
import           Laws
import           Test.QuickCheck.Function
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

-- These definitions are for GHC 7.4
-- GHC 7.4 seems to screw up PolyKinds, so we need to redefine these data types.

newtype Tagged a b = Tagged { untag :: b }
data Proxy a = Proxy

data Void2 a b

-- | A profunctor schedule
type ProS = Schedule (Combine :><: Constant :><: Empty)

-- | A contravariant schedule
type ContraS = Schedule (Empty :><: Combine :><: Reference :><: Limit :><: Repeat)

type QC v = (Arbitrary v, Eq v, Show v)
type Mod m (s :: * -> * -> *) = m s DiscreteTime Int

profunctor :: forall m v. (Profunctor (m ProS), v ~ Mod m ProS, QC v) => Tagged m TestTree
profunctor = Tagged $ testProperty "profunctor" $ isProfunctor (Proxy :: Proxy v)

functor :: forall m v. (Functor (m USchedule DiscreteTime), v ~ Mod m USchedule, QC v) => Tagged m TestTree
functor = Tagged $ testProperty "functor" $ isFunctor (Proxy :: Proxy v)

bifunctor :: forall m v. (Bifunctor (m USchedule), v ~ Mod m USchedule, QC v) => Tagged m TestTree
bifunctor = Tagged $ testProperty "bifunctor" $ isBifunctor (Proxy :: Proxy v)

contravariant :: forall m v. (Contravariant (m ContraS DiscreteTime), v ~ Mod m ContraS, QC v) => Tagged m TestTree
contravariant = Tagged $ testProperty "contravariant" $ isContravariant (Proxy :: Proxy v)

ixed :: forall m v. (v ~ m USchedule DiscreteTime Int, QC v, Int ~ IxValue v, DiscreteTime ~ Index v, Ixed (Bazaar (->) Int Int) v, Contains (Accessor Bool) v) => Tagged m TestTree
ixed = Tagged $ testProperty "ixed" $ isIxed (Proxy :: Proxy (m USchedule DiscreteTime Int))

traversable :: forall m v. (Traversable (m USchedule DiscreteTime), v ~ Mod m USchedule, QC v) => Tagged m TestTree
traversable = Tagged $ testProperty "traversable" $ isTraversal (traverse :: Traversal' (m USchedule DiscreteTime Int) Int)

aeson :: forall m v. (v ~ m USchedule DiscreteTime Int, Function v, QC v, CoArbitrary v, FromJSON v, ToJSON v) => Tagged m TestTree
aeson = Tagged $ testProperty "aeson" $ isAeson (Proxy :: Proxy (m USchedule DiscreteTime Int))

testInstances :: proxy (m :: (* -> * -> *) -> * -> * -> *) -> [Tagged m TestTree] -> [TestTree]
testInstances _ = map untag

--------------------------------------------------------------------------------
--- Empty modifier

test_empty_instances :: [TestTree]
test_empty_instances = testInstances (Proxy :: Proxy Empty) [functor, bifunctor, profunctor, contravariant, ixed, traversable, aeson]

prop_empty_index :: Int -> Bool
prop_empty_index i = isNothing $ (Empty :: Empty USchedule Int Int) ^? ix i

--------------------------------------------------------------------------------
--- Constant modifier

test_constant_instances :: [TestTree]
test_constant_instances = testInstances (Proxy :: Proxy Constant) [functor, bifunctor, profunctor, ixed, traversable, aeson]

prop_constant_index :: Int -> Int -> Bool
prop_constant_index i v = (Constant v :: Constant USchedule Int Int) ^? ix i == Just v

--------------------------------------------------------------------------------
--- Reference modifier

test_reference_instances :: [TestTree]
test_reference_instances = testInstances (Proxy :: Proxy Reference) [functor, bifunctor, contravariant, ixed, traversable, aeson]

prop_reference_index :: USchedule DiscreteTime Int -> DiscreteTime -> DiscreteTime -> Bool
prop_reference_index s i j = reference i s ^? ix j == s ^? ix i

--------------------------------------------------------------------------------
--- Limit modifier

test_limit_instances :: [TestTree]
test_limit_instances = testInstances (Proxy :: Proxy Limit) [functor, bifunctor, contravariant, ixed, traversable, aeson]

prop_limit_index_satisfied :: USchedule DiscreteTime Int -> DiscreteTime -> Property
prop_limit_index_satisfied s b =
     (forAll (arbitrary `suchThat` compared LT) (prop lower)
  .&. forAll (arbitrary `suchThat` compared GT) (prop greater))
 .&&. prop equal b
  where compared c a = compare a b == c
        prop w i = w b s ^? ix i == s ^? ix i

prop_limit_index_unsatisfied :: USchedule DiscreteTime Int -> DiscreteTime -> Property
prop_limit_index_unsatisfied s b =
     forAll (arbitrary `suchThat` ncompared LT) (nprop lower)
 .&. forAll (arbitrary `suchThat` ncompared EQ) (nprop equal)
 .&. forAll (arbitrary `suchThat` ncompared GT) (nprop greater)
  where compared c a = compare a b == c
        ncompared c = not . compared c
        nprop w i = not $ w b s ^. contains i

--------------------------------------------------------------------------------
--- Annotate modifier

test_annotate_instances :: [TestTree]
test_annotate_instances = testInstances (Proxy :: Proxy (Annotate String)) [functor, bifunctor, contravariant, ixed, profunctor, aeson, traversable]

prop_annotate_index :: USchedule DiscreteTime Int -> DiscreteTime -> Bool
prop_annotate_index s i = annotate "Hello world" s ^? ix i == s ^? ix i

--------------------------------------------------------------------------------
--- Repeat modifier

test_repeat_instances :: [TestTree]
test_repeat_instances = testInstances (Proxy :: Proxy Repeat) [functor, bifunctor, contravariant, ixed, aeson, traversable]

prop_repeat_index :: USchedule DiscreteTime Int -> DiscreteTime -> Int -> Bool
prop_repeat_index s i n = all prop [0..pred i]
  where sch = Repeat (i,0) s
        start = i * fromIntegral n
        prop o = sch ^? ix (start <> o) == s ^? ix o

--------------------------------------------------------------------------------
--- Combine modifier

test_combine_instances :: [TestTree]
test_combine_instances = testInstances (Proxy :: Proxy Combine) [functor, bifunctor, contravariant, profunctor, ixed, aeson, traversable]

prop_combine_index :: [USchedule DiscreteTime Int] -> DiscreteTime -> Bool
prop_combine_index s i = sch ^.. ix i == concatMap (toListOf $ ix i) s
  where sch = s ^. combine

--------------------------------------------------------------------------------
--- Enum modifier

test_enum_instances :: [TestTree]
test_enum_instances =
  [ testGroup "Empty :><: Constant" $ testInstances (Proxy :: Proxy (Empty :><: Constant)) [functor, bifunctor, profunctor, ixed, aeson, traversable]
  , testGroup "Empty :><: Combine" $ testInstances (Proxy :: Proxy (Empty :><: Combine)) [functor, bifunctor, profunctor, contravariant, ixed, aeson, traversable]
  ]

properties :: TestTree
properties = $(testGroupGenerator)
