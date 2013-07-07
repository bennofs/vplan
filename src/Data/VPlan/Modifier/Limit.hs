{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Limit a modifier with some predicate on the index
module Data.VPlan.Modifier.Limit (
    Limit()
  , condition
  , bound
  , limited
  , lower
  , equal
  , greater
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At       as A
import           Data.VPlan.Class
import           Data.VPlan.TH

-- | The 'Limit' modifier takes another modifier and behaves like that one, but only when the index compared to
-- the bound gives the 'condition'. If that's not the case, it behaves like the empty modifier.
data Limit s = Limit { _condition :: Ordering, _bound :: Index s, _limited :: s }
makeLenses ''Limit
makeModifier ''Limit
derivePeriodic ''Limit

instance (Ord (Index s), Limited s, Enum (Index s)) => Limited (Limit s) where
  imin (Limit GT b u) = (max (succ b) <$> imin u) <|> (Just $ succ b)
  imin (Limit EQ b _) = Just b
  imin (Limit LT _ u) = imin u
  imax (Limit GT _ u) = imax u
  imax (Limit EQ b _) = Just b
  imax (Limit LT b u) = (min (pred b) <$> imax u) <|> (Just $ pred b)

deriving instance (Data (Index s), Data s) => Data (Limit s)
deriving instance (Eq (Index s), Eq s) => Eq (Limit s)

instance (A.Contains f s, Ord (Index s), Gettable f) => A.Contains f (Limit s) where
  contains i f l
    | compare i (l ^. bound) == l ^. condition = limited (A.contains i f) l
    | otherwise = coerce $ indexed f i False

instance (A.Ixed f s, Applicative f, Ord (Index s)) => A.Ixed f (Limit s) where
  ix i f l
    | compare i (l ^. bound) == l ^. condition = limited (A.ix i f) l
    | otherwise = pure l

-- | Limit another schedule to all indices lower than the given one
lower :: (Ord (Index s)) => Index s -> s -> Limit s
lower = Limit LT

-- | Pick only the value at the given index from anther schedule
equal :: (Ord (Index s)) => Index s -> s -> Limit s
equal = Limit EQ

-- | Limit another schedule to all the indices heigher than the given one
greater :: (Ord (Index s)) => Index s -> s -> Limit s
greater = Limit GT
