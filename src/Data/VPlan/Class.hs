{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds       #-}

-- | Classes that may be implemented by the modifiers to support some features.
module Data.VPlan.Class
  ( Limited(imin,imax)
  , Periodic(interval)
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
  interval :: a -> Index a

  default interval :: (Integral (Index a), Typeable (ScheduleType a), Data a, Periodic (ScheduleType a))
                   => a -> Index a
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
   instance (Integral (Index $t), Data $t, Typeable (ScheduleType $t), Periodic (ScheduleType $t)) => Periodic $t
  |]

-- | Derive all classes in this module
deriveClass :: Name -> Q [Dec]
deriveClass n = concat <$> traverse ($ n) [derivePeriodic, deriveLimited]

-- define these instances here to break import cycle
instance (ModInstance Periodic s i v, ModSame Index s i v) => Periodic (Schedule s i v) where
  interval = interval . review schedule

instance (ModInstance Limited s i v, ModSame Index s i v) => Limited (Schedule s i v) where
  imax = imax . review schedule
  imin = imin . review schedule
