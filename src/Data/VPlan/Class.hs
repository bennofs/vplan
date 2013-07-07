{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleInstances    #-}

-- | Classes that may be implemented by the modifiers to support some features.
module Data.VPlan.Class
  ( Span
  , Limited(imin,imax)
  , Periodic(interval)
  , HasSpan(tmod)
  , derivePeriodic
  , deriveLimited
  , deriveClass
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Data
import           Data.Data.Lens
import           Data.Foldable       (foldl')
import           Data.VPlan.Schedule
import           Language.Haskell.TH
import           Data.VPlan.TH

-- | A class for types that have spans. (Like dates, ...)
class HasSpan a where

  type Span a
  type Span a = a

  -- | Return the time `mod` a given span. This should return the time unchanged when the span is zero. 
  -- It should behave like the haskell `mod` function on negative times/spans (if they exist for the type).
  tmod :: a -> Span a -> a

  default tmod :: (Span a ~ a, Integral a) => a -> a -> a
  tmod x y
    | y == 0 = x
    | otherwise = mod x y

instance HasSpan Integer
instance HasSpan Int
instance (HasSpan a, HasSpan b) => HasSpan (a,b) where
  type Span (a,b) = (Span a, Span b)
  tmod (a,b) = bimap (tmod a) (tmod b)

-- | A class for modifiers that have a max and min index bound.
class Limited a where

  -- | Get the max bound of a value. The default implementation just uses the maximum of all the max indices of the
  -- children's values. A Nothing indicates that there is no maximal index.
  imax :: a -> Maybe (Index a)

  default imax :: (Ord (Index a), Index (ScheduleType a) ~ Index a, Limited (ScheduleType a), Data a,
                  Typeable (ScheduleType a)) => a -> Maybe (Index a)
  imax v
    | c@(_:_) <- childs = fmap maximum . traverse imax $ c
    | otherwise = Nothing
    where childs = v ^.. template :: [ScheduleType a]

  -- | Get the min bound of a type. The default implementation just uses the minimum of all the min indices of the
  -- children's values. A Nothing indicates that there is no minimal indices.
  imin :: a -> Maybe (Index a)

  default imin :: (Ord (Index a), Index (ScheduleType a) ~ Index a, Limited (ScheduleType a), Data a,
                  Typeable (ScheduleType a)) => a -> Maybe (Index a)
  imin v
    | c@(_:_) <- childs = fmap minimum . traverse imin $ c
    | otherwise = Nothing
    where childs = v ^.. template :: [ScheduleType a]


-- | A modifier that repeats in a specific interval.
class Periodic a where

  -- | The interval after which the modifier repeats. The interval may only be valid within the index range
  -- (gminBound a, gmaxBound a).  The default implementation takes the lcm of all the individual intervals of all the
  -- children schedules. If there are no children schedules, the interval is assumed to be 1.
  interval :: a -> Span (Index a)

  default interval :: (Integral (Span (Index a)), Typeable (ScheduleType a), Data a, Periodic (ScheduleType a))
                   => a -> Span (Index a)
  interval a
    | (h:t) <- ms = foldl' (flip $ lcm . interval) (interval h) t
    | otherwise = 1
    where ms = a ^.. template :: [ScheduleType a]

-- | Generate Limited instances
deriveLimited :: Name -> Q [Dec]
deriveLimited n = let t = resolveType n in
  [d|
   instance (Data $t, Ord (Index $t), Typeable (ScheduleType $t), Limited (ScheduleType $t)) => Limited $t
  |]

-- | Generate Periodic instances
derivePeriodic :: Name -> Q [Dec]
derivePeriodic n = let t = resolveType n in
  [d|
   instance (Integral (Span (Index $t)), Data $t, Typeable (ScheduleType $t), Periodic (ScheduleType $t)) => Periodic $t
  |]

-- | Derive all classes in this module
deriveClass :: Name -> Q [Dec]
deriveClass n = concat <$> traverse ($ n) [derivePeriodic, deriveLimited]

-- define these instances here to break import cycle
instance (Periodic (s (Schedule i v s)), i ~ Index (s (Schedule i v s))) => Periodic (Schedule i v s) where
  interval = interval . review schedule
instance (Limited (s (Schedule i v s)), i ~ Index (s (Schedule i v s))) => Limited (Schedule i v s) where
  imax = imax . review schedule
  imin = imin . review schedule
