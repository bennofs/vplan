{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core.Timespan (
  Timespan(), Transformed(), 
  timespan, as, toSeconds, transformBy, runTrans,
  seconds, hours, minutes, days, weeks) where

import Control.Arrow
import Control.Applicative

newtype Transformed a = Trans (a->a, a)

transformBy :: (a -> a) -> a -> Transformed a
transformBy = curry Trans

runTrans :: Transformed a -> a
runTrans ~(Trans ~(f,x)) = f x

instance Functor Transformed where
  fmap f ~(Trans ~(f',x)) = Trans (id,f $ f' x)
  
instance Applicative Transformed where
  pure = curry Trans id
  ~(Trans ~(f,x)) <*> ~(Trans ~(g,y)) = Trans (id, f x $ g y)
  
instance Monad Transformed where
  return = pure
  ~(Trans ~(t,x)) >>= f = f $ t x 

type Timespan a = Transformed a

timespan :: a -> (a -> a) -> Timespan a
timespan = flip as

as :: (a -> a) -> a -> Timespan a
as = transformBy

toSeconds :: Timespan a -> a
toSeconds = runTrans

seconds :: (Num a) => a -> a
minutes :: (Num a) => a -> a
hours :: (Num a) => a -> a
days :: (Num a) => a -> a
weeks :: (Num a) => a -> a
seconds = id
minutes = (*60) . seconds
hours = (*60) . minutes
days = (*24) . hours
weeks = (*7) . days

instance (Num a) => Num (Transformed a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = liftA abs
  signum = liftA signum
  fromInteger = as seconds . fromInteger

instance (Bounded a) => Bounded (Transformed a) where
  minBound = pure minBound
  maxBound = pure maxBound

instance (Enum a) => Enum (Transformed a) where
  toEnum = pure . toEnum
  fromEnum = fromEnum . runTrans

instance (Eq a) => Eq (Transformed a) where
  (==) = curry $ runTrans . uncurry (liftA2 (==))
  
instance (Ord a) => Ord (Transformed a) where
  (<) = curry $ runTrans . uncurry (liftA2 (<))

instance (Real a) => Real (Transformed a) where
  toRational = runTrans . fmap toRational

instance (Integral a) => Integral (Transformed a) where
  quotRem = curry $ (pure *** pure) . uncurry quotRem . (runTrans *** runTrans)
  toInteger = toInteger . runTrans