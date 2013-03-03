{-# LANGUAGE TemplateHaskell
 , GeneralizedNewtypeDeriving
 , TupleSections
 , UndecidableInstances
 , FlexibleInstances
 , OverlappingInstances
 , NoImplicitPrelude
 , FunctionalDependencies #-}

-- | 
-- Provides datestructures and utilities for working with time and dates.

module Core.Time (
  {-|
    This module provides the data types and lenses for working with times and dates.

    The core data type for dates is 'Date'. It stores the date in weekdate format, accessible with the lenses 'dateWeek' and 'dateDay':

    >>> date 1 Monday ^. dateWeek :: Week
    Week 1

    >>> date 1 Monday ^. dateDay :: Day
    Monday



    The module features a strict separation between timespans and dates, the only way to convert between the two is the' daysFromOrigin' lens.

    >>> date 1 Tuesday + date 1 Monday -- Dates can't be added, that wouldn't make sense
        No instance for (Additive.C Date) arising from a use of `+' ...

    >>> (3 ^. days) `add` (2 ^. weeks) :: Days
    Days 17

    >>> (2 ^. weeks) `add` (1 ^. days) -- Can't add days to weeks, that would result in a loss of precision (because the result is also in weeks)
        No instance for (HasDaysIso Weeks) arising from a use of `add' ...



    There are also serveral operations on dates, such as going forward or backward:

    >>> forward (1 ^. weeks) $ date 1 Monday :: Date
    Date (Week 2,Monday)

    >>> backward (7 ^. days) $ date 1 Monday :: Date
    Date (Week 0,Monday)

  -}
  
    Day (..)
  , Days (), days, daysPrism, daysIso, toDays
  , Week (), week
  , Weeks (), weeks
  , Date, daysFromOrigin, date, dateWeek, dateDay
  , convertTimespanIso, forward, backward, add, sub, difference
  )
       where

import Control.Arrow
import Control.Lens
import NumericPrelude
import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module

-- | A day of the week
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Enum, Bounded, Read, Show)

-- | A timespan in days. 
newtype Days = Days Int deriving (Eq, Ord, Read, Show, Enum, Additive.C)
makeIso ''Days

instance Module.C Int Days where
  a *> (Days b) = Days $ a * b

-- | A type for week. A week is, like a day, absolute and should not be used to store timespans. Use Weeks for that purpose.
newtype Week = Week Int deriving (Eq, Ord, Read, Show, Enum)
makeIso ''Week

-- | A timespan in weeks.
newtype Weeks = Weeks Int deriving (Enum, Eq, Ord, Read, Show, Additive.C)
makeIso ''Weeks

instance Module.C Int Weeks where
  a *> (Weeks b) = Weeks $ a * b

-- | A date in weekdate format.
newtype Date = Date (Week, Day) deriving (Show, Eq, Ord, Read)

-- | Contruct a date from a week number and a day.
date :: Int -> Day -> Date
date = curry $ Date . (_1 %~ Week)

-- | Get the week of a date
dateWeek :: Lens' Date Week
dateWeek = lens (\(Date (x,_)) -> x) $ \(Date (_,y)) x -> Date (x,y)

-- | Get the day of a date
dateDay :: Lens' Date Day
dateDay = lens (\(Date (_,y)) -> y) $ \(Date (x,_)) y -> Date (x,y)

-- | A timespan in weekdate format
newtype DateTimespan = DateTimespan (Weeks, Days) deriving (Ord, Show, Eq, Read)
makeIso ''DateTimespan

instance Additive.C DateTimespan where
  t + s = t `add` s
  t - s = sub s t
  zero = 0 ^. days . from daysIso

instance Module.C Int DateTimespan where
  a *> b = review daysIso $ a *> (b ^. toDays)

-- | An iso from a date to a timespan from the epoch.
daysFromOrigin :: Iso' Date Days
daysFromOrigin = iso t f
  where t (Date (Week w, d)) = (w * 7 + fromEnum d) ^. days
        f d = let (Weeks w, Days x) = d ^. re daysIso . from dateTimespan in Date (Week w, toEnum x)


-- | Overload daysPrism
class HasDaysPrism a where
  
  -- | A Prism from days to a
  daysPrism :: Prism' Days a
  
instance HasDaysPrism Weeks where
  daysPrism = prism' (view days . (* 7) . review weeks) undefined

instance (HasDaysIso a) => HasDaysPrism a where
  daysPrism = prism' (view daysIso) (Just . review daysIso)

-- | Overload daysIso
class HasDaysIso a where
  
  -- | An iso between a and days
  daysIso :: Iso' a Days
  
instance HasDaysIso Int where
  daysIso = days
  
instance HasDaysIso Days where
  daysIso = iso id id
  
instance HasDaysIso DateTimespan where
  daysIso = iso t f 
    where f d = DateTimespan $ Weeks *** Days $ (d ^. from days) `divMod` 7
          t d = uncurry (+) $ d ^. from dateTimespan & _1 %~ (view days . (* 7) . review weeks)

-- | Convert a timespan to days
toDays :: (HasDaysPrism a) => Getter a Days
toDays = re daysPrism

-- | Convert from one representation of a timespan to another
convertTimespanIso :: (HasDaysIso t, HasDaysIso s) => Iso' t s
convertTimespanIso = iso (review daysIso . view daysIso) (review daysIso . view daysIso)

-- | Add a timespan to a date
forward :: (HasDaysPrism s) => s -> Date -> Date
forward s = daysFromOrigin %~ (+ s ^. toDays)

-- | Subtract a timespan from a date
backward :: (HasDaysPrism s) => s -> Date -> Date
backward s = daysFromOrigin %~ (subtract $ s ^. toDays)

-- | Add two timspans, this is to ease adding timespans of different types.
add :: (HasDaysPrism s, HasDaysIso t) => t -> s -> t
add = flip $ \s -> daysIso %~ (+ s ^. toDays)

-- | Subtract a timespan from another one. This function exists to ease working with timespans of different types.
sub :: (HasDaysPrism s, HasDaysIso t) => t -> s -> t
sub = flip $ \s -> daysIso %~ (flip subtract $ s ^. toDays)

-- | Calculate the difference between to dates in days
difference :: Date -> Date -> Days
difference d e = e ^. daysFromOrigin - d ^. daysFromOrigin