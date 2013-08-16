{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | A representation of time and time spans compatible with VPlan.
module Data.VPlan.Time where

import           Control.Lens
import           Data.Aeson
import           Data.Group
import           Data.Semigroup

-- | A discrete time value. There exists a smallest time unit and there exists a unique successor and predecessor for all values.
newtype DiscreteTime = DiscreteTime { getDiscreteTime :: Sum Integer } deriving (Monoid, Group, Semigroup, Eq, Ord)

instance ToJSON DiscreteTime where
  toJSON = Number . fromInteger . view _DiscreteTime

instance FromJSON DiscreteTime where
  parseJSON = withNumber "Integer" (return . review _DiscreteTime . floor)

instance Enum DiscreteTime where
  toEnum = review $ _DiscreteTime . unto toInteger
  fromEnum = view $ _DiscreteTime . to fromInteger

-- | Isomorphism between the DiscreteTime and the underlying time value.
_DiscreteTime :: Iso' DiscreteTime Integer
_DiscreteTime = iso (getSum . getDiscreteTime) (DiscreteTime . Sum)

-- | A continuous time value. There is no smallest unit and there is always another
-- time value between two given different time values.
newtype ContinuousTime = ContinuousTime { getContinuousTime :: Sum Rational } deriving (Monoid, Group, Semigroup, Eq, Ord)

instance ToJSON ContinuousTime where
  toJSON = Number . fromRational . view _ContinuousTime

instance FromJSON ContinuousTime where
  parseJSON = withNumber "Rational" (return . review _ContinuousTime . toRational)

-- | Isomorphism between the ContinuousTime and the underlying time value.
_ContinuousTime :: Iso' ContinuousTime Rational
_ContinuousTime = iso (getSum . getContinuousTime) (ContinuousTime . Sum)

newtype Day = Day { getDay :: DiscreteTime } deriving (Monoid, Group, Semigroup, Eq, Ord, Enum)

-- | Isomorphism between the day index (monday is zero) and a Day value.
_Day :: Iso' Day Integer
_Day = iso (view _DiscreteTime . getDay) (Day . review _DiscreteTime)

monday, tuesday, wednesday, thursday, friday, saturday, sunday :: Day
monday    = _Day # 0
tuesday   = _Day # 1
wednesday = _Day # 2
thursday  = _Day # 3
friday    = _Day # 4
saturday  = _Day # 5
sunday    = _Day # 6

-- | A date represented by the week number and a day in the week.
data WeekDate = WeekDate { _weekdateWeek :: Integer, _weekdateDay :: Day }
makeFields ''WeekDate

instance Semigroup WeekDate

instance Monoid WeekDate where
  mempty = WeekDate 0 mempty
  mappend a b = WeekDate (a ^. week + b ^. week) (a ^. day <> b ^. day)

instance Group WeekDate where
  invert a = a & week %~ negate & day %~ invert
