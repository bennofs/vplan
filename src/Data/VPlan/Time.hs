{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | A representation of time and time spans compatible with VPlan.
module Data.VPlan.Time where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Data
import           Data.Group
import           Data.Semigroup

deriving instance Typeable1 Sum
deriving instance (Data a) => Data (Sum a)

-- | A discrete time value. There exists a smallest time unit and there exists a unique successor and predecessor for all values.
newtype DiscreteTime = DiscreteTime { getDiscreteTime :: Sum Integer } deriving (Monoid, Group, Semigroup, Eq, Ord, Abelian, Show, Typeable, Data, Read)

instance Num DiscreteTime where
  (+) = (<>)
  a - b = _DiscreteTime -~ (b ^. _DiscreteTime) $ a
  (*) a = _DiscreteTime *~ (a ^. _DiscreteTime)
  fromInteger = review _DiscreteTime
  abs = _DiscreteTime %~ abs
  signum = _DiscreteTime %~ signum
  negate = invert

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
newtype ContinuousTime = ContinuousTime { getContinuousTime :: Sum Rational } deriving (Monoid, Group, Semigroup, Eq, Ord, Abelian, Show, Typeable, Data, Read)

instance Num ContinuousTime where
  (+) = (<>)
  a - b = _ContinuousTime -~ (b ^. _ContinuousTime) $ a
  (*) a = _ContinuousTime *~ (a ^. _ContinuousTime)
  negate = invert
  abs = over _ContinuousTime abs
  signum = over _ContinuousTime signum
  fromInteger = fromRational . fromInteger

instance Fractional ContinuousTime where
  a / b = _ContinuousTime //~ (b ^. _ContinuousTime) $ a
  recip = over _ContinuousTime recip
  fromRational = review _ContinuousTime

instance ToJSON ContinuousTime where
  toJSON = Number . fromRational . view _ContinuousTime

instance FromJSON ContinuousTime where
  parseJSON = withNumber "Rational" (return . review _ContinuousTime . toRational)

-- | Isomorphism between the ContinuousTime and the underlying time value.
_ContinuousTime :: Iso' ContinuousTime Rational
_ContinuousTime = iso (getSum . getContinuousTime) (ContinuousTime . Sum)

-- | Represents WeekDay. This representation assumes 7-day weeks.
newtype WeekDay = WeekDay { getWeekDay :: Int } deriving (Eq, Ord, ToJSON, FromJSON, Show, Data, Typeable, Read)

-- | Isomorphism between the day index (monday is zero) and a Day value.
_WeekDay :: Iso' WeekDay Int
_WeekDay = iso ((`mod` 7) . getWeekDay) (WeekDay . (`mod` 7))

instance Semigroup WeekDay
instance Abelian WeekDay

instance Monoid WeekDay where
  mempty = WeekDay 0
  mappend a b = review _WeekDay $ a ^. _WeekDay + b ^. _WeekDay

instance Group WeekDay where
  invert = over _WeekDay negate

instance Enum WeekDay where
  fromEnum = view _WeekDay
  toEnum = review _WeekDay

monday, tuesday, wednesday, thursday, friday, saturday, sunday :: WeekDay
monday    = _WeekDay # 0
tuesday   = _WeekDay # 1
wednesday = _WeekDay # 2
thursday  = _WeekDay # 3
friday    = _WeekDay # 4
saturday  = _WeekDay # 5
sunday    = _WeekDay # 6

-- | A date represented by the week number and a day in the week.
data WeekDate = WeekDate { _weekdateWeek :: Integer, _weekdateDay :: WeekDay } deriving (Eq, Ord, Show, Data, Typeable, Read)
makeFields ''WeekDate

instance Semigroup WeekDate
instance Abelian WeekDate

instance Monoid WeekDate where
  mempty = WeekDate 0 mempty
  mappend a b = WeekDate (a ^. week + b ^. week) (a ^. day <> b ^. day)

instance Group WeekDate where
  invert a = a & week %~ negate & day %~ invert

instance Enum WeekDate where
  fromEnum date = fromInteger (date ^. week) * 7 + (date ^. day . _WeekDay)
  toEnum x = let (w,d) = x `divMod` 7 in WeekDate (toInteger w) (WeekDay d)

instance ToJSON WeekDate where
  toJSON o = object [ "week" .= (o ^. week), "day" .= (o ^. day)]

instance FromJSON WeekDate where
  parseJSON = withObject "WeekDate" $ \o -> WeekDate <$> o .: "week" <*> o .: "day"
