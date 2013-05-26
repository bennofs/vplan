{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleInstances    #-}
-- |
-- Module      : $Header$
-- Description : Classes that may be implemented by the modifiers to support some features.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (uses various GHC extensions)

module Data.VPlan.Class
  ( Span
  , Limited
  , Periodic
  , HasSpan
  , tmod
  , imax
  , imin
  , interval
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
import           Control.Monad

-- | A class for types that have spans. (Like dates, ...)
class HasSpan a where
  type Span a
  type Span a = a

  tmod :: a -> Span a -> a

  default tmod :: (Span a ~ a, Integral a) => a -> a -> a
  tmod = mod

-- | Overlapping instance
instance (Integral a) => HasSpan a

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


-- | A modifier that repeats in a specific interval. Note that a non-periodic modifier can still be made an instance of
-- this class by using the interval 0.
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

-- | Get the type to a given name
resolveType :: Name -> Q Type
resolveType = reify >=> getTCInfo >=> \(n, tv) -> foldl appT (conT n) $ map (varT . getTVName) tv

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

instance (Integral (Span i), Data (Schedule i v s)) => Periodic (Schedule i v s)
instance (Ord i, Typeable (Schedule i v s), Data (s (Schedule i v s))) => Limited (Schedule i v s)
