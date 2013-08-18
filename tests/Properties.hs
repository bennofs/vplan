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
module Properties where

import           Control.Lens
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.Tagged
import           Data.VPlan
import           Instances             ()
import           Laws
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

data Void2 a b

-- | A profunctor schedule
type ProS = Schedule :$ Combine :><: Constant :><: Empty

-- | A contravariant schedule
type ContraS = Schedule :$ Combine :><: Empty :><: Reference :><: Limit :><: Repeat

type QC v = (Arbitrary v, Eq v, Show v)
type QCMod m (s :: * -> * -> *) = QC (m s Int Int)

profunctor :: forall m. (Profunctor (m ProS), QCMod m ProS) => Tagged m Property
profunctor = Tagged $ isProfunctor (Proxy :: Proxy (m ProS))

functor :: forall m. (Functor (m USchedule Int), QCMod m USchedule) => Tagged m Property
functor = Tagged $ isFunctor (Proxy :: Proxy (m USchedule Int))

bifunctor :: forall m. (Bifunctor (m USchedule), QCMod m USchedule) => Tagged m Property
bifunctor = Tagged $ isBifunctor (Proxy :: Proxy (m USchedule))

contravariant :: forall m. (Contravariant (m ContraS Int), QCMod m ContraS) => Tagged m Property
contravariant = Tagged $ isContravariant (Proxy :: Proxy (m ContraS Int))

ixed :: forall m v. (v ~ m USchedule DiscreteTime Int, QC v, Int ~ IxValue v, DiscreteTime ~ Index v, Ixed (Bazaar (->) Int Int) v, Contains (Accessor Bool) v) => Tagged m Property
ixed = Tagged $ isIxed (Proxy :: Proxy (m USchedule DiscreteTime Int))

checkInstances :: proxy (m :: (* -> * -> *) -> * -> * -> *) -> [Tagged m Property] -> Property
checkInstances _ = foldl' (.&.) (property True) . map untag

--------------------------------------------------------------------------------
--- Empty modifier

prop_empty_instances :: Property
prop_empty_instances = checkInstances (Proxy :: Proxy Empty) [functor, bifunctor, profunctor, contravariant, ixed]

prop_empty_index :: Int -> Bool
prop_empty_index i = isNothing $ (Empty :: Empty USchedule Int Int) ^? ix i

--------------------------------------------------------------------------------
--- Constant modifier

prop_constant_instances :: Property
prop_constant_instances = checkInstances (Proxy :: Proxy Constant) [functor, bifunctor, profunctor, ixed]

prop_constant_index :: Int -> Int -> Bool
prop_constant_index i v = (Constant v :: Constant USchedule Int Int) ^? ix i == Just v

--------------------------------------------------------------------------------
--- Reference modifier

prop_reference_instances :: Property
prop_reference_instances = checkInstances (Proxy :: Proxy Reference) [functor, bifunctor, contravariant, ixed]

--------------------------------------------------------------------------------
--- Limit modifier

prop_limit_instances :: Property
prop_limit_instances = checkInstances (Proxy :: Proxy Limit) [functor, bifunctor, contravariant, ixed]

prop_limit_index :: USchedule DiscreteTime Int -> DiscreteTime -> Property
prop_limit_index s b =
     forAll (arbitrary `suchThat` compared LT) (prop lower)
 .&. forAll (arbitrary `suchThat` compared GT) (prop greater)
 .&. forAll (arbitrary `suchThat` ncompared LT) (expectFailure . prop lower)
 .&. forAll (arbitrary `suchThat` ncompared EQ) (expectFailure . prop equal)
 .&. forAll (arbitrary `suchThat` ncompared GT) (expectFailure . prop greater)
 .&. prop equal b
  where compared c a = compare a b == c
        ncompared c = not . compared c
        prop w i = w b s ^? ix i == s ^? ix i

--------------------------------------------------------------------------------
--- Annotate modifier

prop_annotate_instances :: Property
prop_annotate_instances = checkInstances (Proxy :: Proxy (Annotate String)) [functor, bifunctor, contravariant, ixed, profunctor]

prop_annotate_index :: USchedule DiscreteTime Int -> DiscreteTime -> Bool
prop_annotate_index s i = annotate "Hello world" s ^? ix i == s ^? ix i

--------------------------------------------------------------------------------
--- Repeat modifier

prop_repeat_instances :: Property
prop_repeat_instances = checkInstances (Proxy :: Proxy Repeat) [functor, bifunctor, contravariant, ixed]

--------------------------------------------------------------------------------
--- Enum modifier

prop_enum_instances :: Property
prop_enum_instances = checkInstances (Proxy :: Proxy (Empty :><: Constant)) [functor, bifunctor, profunctor, ixed]
                  .&. checkInstances (Proxy :: Proxy (Empty :><: Combine)) [functor, bifunctor, profunctor, contravariant, ixed]

prop_enum_index :: Combine (Schedule (Combine :><: AllModifiers)) DiscreteTime Int -> Combine (Schedule (AllModifiers :><: Combine)) DiscreteTime Int -> DiscreteTime -> Bool
prop_enum_index a b i = (Schedule (L a) :: Schedule (Combine :><: AllModifiers) DiscreteTime Int) ^? ix i == a ^? ix i
                      && (Schedule (R b) :: Schedule (AllModifiers :><: Combine) DiscreteTime Int) ^? ix i == b ^? ix i

properties :: TestTree
properties = $(testGroupGenerator)
