-- | Classes that may be implemented by the modifiers to support some features.
module Data.VPlan.Class
  ( Limited(imin,imax)
  , Periodic(interval)
  ) where

import           Control.Lens

-- | A class for modifiers that have a max and min index bound.
class Limited a where

  -- | Get the max bound of a value. Nothing indicates that there is no maximal index.
  imax :: a -> Maybe (Index a)

  -- | Get the min bound of a type. The value Nothing indicates that there is no minimal indices.
  imin :: a -> Maybe (Index a)

-- | A modifier that repeats in a specific interval.
class Periodic a where

  -- | The interval after which the modifier repeats. The interval may only be valid within the index range
  -- (imin a, imax a).
  interval :: a -> Index a


