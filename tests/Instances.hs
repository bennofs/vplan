{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | QuickCheck instances for data types defined in this package
module Instances where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.VPlan
import           Test.QuickCheck

instance (Arbitrary :$ s (Schedule s) i v, Supported Empty (Schedule s)) => Arbitrary (Schedule s i v) where
  arbitrary = sized $ \s ->
    if s < 5 then pure blank else resize (min s 70) $ Schedule <$> arbitrary
  shrink (Schedule s) = [blank] ++ do Schedule <$> shrink s

--------------------------------------------------------------------------------
-- Arbitrary instances for time values

instance Arbitrary DiscreteTime where arbitrary = fmap (review _DiscreteTime) arbitrary
instance Arbitrary ContinuousTime where arbitrary = fmap (review _ContinuousTime) arbitrary

decreaseSize :: Gen a -> Gen a
decreaseSize g = sized $ \s -> if s < 5 then g else resize (s-1) g

--------------------------------------------------------------------------------
-- Arbitrary instances for Modifiers

deriving instance (Arbitrary v) => Arbitrary (Constant s i v)
instance Arbitrary (Empty s i v) where arbitrary = pure Empty

instance (Arbitrary :$ s i v, Arbitrary a) => Arbitrary (Annotate a s i v) where
  arbitrary = decreaseSize $ annotate <$> arbitrary <*> arbitrary
  shrink = attached shrink >=> annotated shrink

instance (Arbitrary :$ s i v, Arbitrary i, Ord i) => Arbitrary (Limit s i v) where
  arbitrary = decreaseSize $ frequency $ map (_2 %~ \x -> liftA2 x arbitrary arbitrary)
    [ (50, equal)
    , (25, lower)
    , (25, greater)
    ]
  shrink = bound shrink >=> condition shrink >=> limited shrink

instance (Arbitrary :$ s i v, Arbitrary i) => Arbitrary (Reference s i v) where
  arbitrary = decreaseSize $ reference <$> arbitrary <*> arbitrary
  shrink = referenced shrink

instance (Arbitrary :$ s i v, Arbitrary i, Ord i, Num i) => Arbitrary (Repeat s i v) where
  arbitrary = decreaseSize $ Repeat <$> minmax <*> arbitrary
    where minmax = do
            x <- abs <$> arbitrary
            y <- fmap abs arbitrary `suchThat` (/= x)
            return (max x y, min x y)

instance (Arbitrary :$ s i v) => Arbitrary (Combine s i v) where
  arbitrary = decreaseSize $ combine <$> list
    where list = frequency ([(1,0),(2,1),(5,1),(3,4)] & traversed . _2 %~ pure) >>= flip vectorOf arbitrary
  shrink (Combine l) = Combine <$> shrink l

instance (BothInstance Arbitrary a b s i v) => Arbitrary (C a b s i v) where
  arbitrary = frequency
    [ (1, L <$> arbitrary)
    , (3, R <$> arbitrary)
    ]
  shrink (L a) = L <$> shrink a
  shrink (R a) = R <$> shrink a
