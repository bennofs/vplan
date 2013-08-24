{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | QuickCheck instances for data types defined in this package
module Instances where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.VPlan hiding ((:$))
import           GHC.Real
import           Test.QuickCheck
import           Test.QuickCheck.Function

type a :$ b = a b
infixr 0 :$

instance (Arbitrary :$ s (Schedule s) i v, Supported Empty (Schedule s)) => Arbitrary (Schedule s i v) where
  arbitrary = sized $ \s ->
    if s < 5 then pure blank else resize (min s 50) $ Schedule <$> arbitrary
  shrink (Schedule s) = blank : do Schedule <$> shrink s

instance (Function :$ s (Schedule s ) i v) => Function (Schedule s i v) where function = functionMap (review schedule) (view schedule)
instance (CoArbitrary :$ s (Schedule s) i v) => CoArbitrary (Schedule s i v) where coarbitrary = reviews schedule coarbitrary

--------------------------------------------------------------------------------
-- Arbitrary instances for time values

instance Arbitrary DiscreteTime where arbitrary = fmap (review _DiscreteTime) arbitrary
instance Arbitrary ContinuousTime where arbitrary = fmap (review _ContinuousTime) arbitrary

decreaseSize :: Gen a -> Gen a
decreaseSize g = sized $ \s -> if s < 5 then g else resize (s-1) g

--------------------------------------------------------------------------------
-- Function instances for time values

instance Function DiscreteTime where function = functionMap (view _DiscreteTime) (review _DiscreteTime)
instance Function ContinuousTime where function = functionMap (view _ContinuousTime) (review _ContinuousTime)

--------------------------------------------------------------------------------
-- CoArbitrary instances for time values
                                       
instance CoArbitrary DiscreteTime where coarbitrary = views _DiscreteTime coarbitrary
instance CoArbitrary ContinuousTime where coarbitrary = views _ContinuousTime coarbitrary

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
  arbitrary = decreaseSize $ view combine <$> list
    where list = frequency ([(1,0),(2,1),(5,1),(3,4)] & traversed . _2 %~ pure) >>= flip vectorOf arbitrary
  shrink = from combine shrink

instance (BothInstance Arbitrary a b s i v) => Arbitrary (C a b s i v) where
  arbitrary = frequency
    [ (1, L <$> arbitrary)
    , (3, R <$> arbitrary)
    ]
  shrink (L a) = L <$> shrink a
  shrink (R a) = R <$> shrink a

--------------------------------------------------------------------------------
-- Function instances

instance Function v => Function (Constant s i v)         where function = functionMap (review constant) (view constant)
instance Function (Empty s i v)                          where function = functionMap (const ()) (const Empty)
instance (Function [s i v]) => Function (Combine s i v)  where function = functionMap (review combine) (view combine)
instance Function Ordering                               where function = functionShow

instance (Function a, Function (s i v)) => Function (Annotate a s i v) where
  function = functionMap (\(Annotate a s) -> (a,s)) $ uncurry Annotate

instance (Function i, Function (s i v)) => Function (Limit s i v) where
  function = functionMap (\l -> (l ^. bound, l ^. condition, l ^. limited)) (\(b,c,l) -> limit c b l)

instance (Function i, Function (s i v)) => Function (Repeat s i v) where
  function = functionMap (\(Repeat i c) -> (i,c)) $ uncurry Repeat

instance (BothInstance Function a b s i v) => Function (C a b s i v) where
  function = functionMap t $ either L R
    where t (L x) = Left x
          t (R x) = Right x

instance (Function i, Function (s i v)) => Function (Reference s i v) where
  function = functionMap (\r -> (r ^. source, r ^. referenced)) $ uncurry reference

--------------------------------------------------------------------------------
-- CoArbitrary instances for modifiers

instance CoArbitrary v => CoArbitrary (Constant s i v) where coarbitrary = reviews constant coarbitrary
instance CoArbitrary (Empty s i v) where coarbitrary = const id
instance CoArbitrary (s i v) => CoArbitrary (Combine s i v) where coarbitrary = reviews combine coarbitrary
instance (CoArbitrary i, CoArbitrary (s i v)) => CoArbitrary (Reference s i v) where coarbitrary (Reference i s) = coarbitrary (i,s)
instance (CoArbitrary i, CoArbitrary (s i v)) => CoArbitrary (Limit s i v) where coarbitrary l = coarbitrary (l ^. bound, l ^. condition, l ^. limited)
instance (CoArbitrary i, CoArbitrary (s i v)) => CoArbitrary (Repeat s i v) where coarbitrary (Repeat i s) = coarbitrary (i,s)
instance (CoArbitrary (s i v), CoArbitrary a) => CoArbitrary (Annotate a s i v) where coarbitrary (Annotate a s) = coarbitrary (a,s)
instance (BothInstance CoArbitrary a b s i v) => CoArbitrary (C a b s i v) where
  coarbitrary (L a) = variant (0 :: Integer) . coarbitrary a
  coarbitrary (R a) = variant (-1 :: Integer) . coarbitrary a


--------------------------------------------------------------------------------
-- Arbitrary instances for other types

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink = fmap BS.pack . shrink . BS.unpack

instance Arbitrary LBS.ByteString where
  arbitrary = LBS.pack <$> arbitrary
  shrink = fmap LBS.pack . shrink . LBS.unpack

instance Function Rational where
  function = functionMap (\(a :% b) -> (a,b)) $ uncurry (%)
