{-# LANGUAGE TemplateHaskell
 , GeneralizedNewtypeDeriving
 , TupleSections
 , FlexibleInstances
 , MultiParamTypeClasses
 , NoImplicitPrelude
 #-}

-- |
-- Module      : $Header$
-- Description : Type-safe utilities for working with dates and times in weekdate format
--               in a calendar that doesn't have to deal with real-world times and because of that
--               can be a lot simpler than the one in Data.Time.Calendar.WeekDate.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (Only works in GHC because of lens)
module Core.Time (
  {-|
    * Usage:

    This module provides the data types and lenses for working with times and dates.

    The core data type for dates is 'Date'. It stores the date in weekdate format,
    accessible by the lenses 'dateWeek' and 'dateDay':

    >>> date 1 Monday ^. dateWeek :: Week
    Week 1

    >>> date 1 Monday ^. dateDay :: Day
    Monday

    The module features a strict separation between timespans and dates, the only way to
    convert between the two is the' daysFromOrigin' lens.

    >>> date 1 Tuesday + date 1 Monday -- Dates can't be added, that wouldn't make sense
        No instance for (Additive.C Date) arising from a use of `+' ...

    >>> (3 ^. days) `add` (2 ^. weeks) :: Days
    Days 17

    >>> (2 ^. weeks) `add` (1 ^. days) -- Can't add days to weeks, that would result in a loss of precision (because the result would also be in weeks)
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
  , DateTimespan, dateTimespan, dtimespan
  , HasDaysIso, HasDaysPrism
  , convertTimespan, convertTimespanIso, forward, backward, add, sub, mult, difference, lcs
  )
       where

import Data.List (foldl')
import Control.Applicative hiding ((*>))
import Control.Monad
import Control.Arrow
import Control.Lens
import NumericPrelude hiding ((^?), concat)
import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module
import qualified Algebra.ToInteger as ToInteger

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

-- | Construct a 'DateTimespan' from a timespan in weeks and a timespan in days. The timespan in
-- days should be lower than 7.
dtimespan :: Weeks -> Days -> DateTimespan
dtimespan = curry DateTimespan

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

instance Enum Date where
  toEnum x = x ^. days . from daysFromOrigin
  fromEnum x = x ^. daysFromOrigin . from days

-- | Overload daysPrism
class HasDaysPrism a where

  -- | A Prism from days to a
  daysPrism :: Prism' Days a

instance HasDaysPrism Weeks where
  daysPrism = prism' (view days . (* 7) . review weeks) (t . review days)
    where t x = let (m,r) = x `divMod` 7 in m ^. weeks <$ guard (r == 0)

instance HasDaysPrism Days where daysPrism = defaultDaysPrism
instance HasDaysPrism DateTimespan where daysPrism = defaultDaysPrism

-- | A default value for daysPrism for types that are an instance of HasDaysIso
defaultDaysPrism :: (HasDaysIso a) => Prism' Days a
defaultDaysPrism = prism' (view daysIso) (Just . review daysIso)

-- | Overload daysIso
class (HasDaysPrism a) => HasDaysIso a where

  -- | An iso between a and days
  daysIso :: Iso' a Days

instance HasDaysIso Days where
  daysIso = iso id id

instance HasDaysIso DateTimespan where
  daysIso = iso t f
    where f d = DateTimespan $ Weeks *** Days $ (d ^. from days) `divMod` 7
          t d = uncurry (+) $ d ^. from dateTimespan & _1 %~ (view days . (* 7) . review weeks)


toDays :: (HasDaysPrism a) => Getter a Days
toDays = re daysPrism

-- | Convert from one representation of a timespan to another
convertTimespan :: (HasDaysIso s, HasDaysPrism t) => Getter t s
convertTimespan = toDays . from daysIso

-- | Convert from one representation of a timespan to another
convertTimespanIso :: (HasDaysIso t, HasDaysIso s) => Iso' t s
convertTimespanIso = iso (review daysIso . view daysIso) (review daysIso . view daysIso)

-- | Add a timespan to a date
forward :: (HasDaysPrism s) => s -> Date -> Date
forward s = daysFromOrigin %~ (+ s ^. toDays)

-- | Subtract a timespan from a date
backward :: (HasDaysPrism s) => s -> Date -> Date
backward s = daysFromOrigin %~ subtract (s ^. toDays)

-- | Add two timspans, this is to ease adding timespans of different types.
add :: (HasDaysPrism s, HasDaysIso t) => t -> s -> t
add = flip $ \s -> daysIso %~ (+ s ^. toDays)

-- | Subtract a timespan from another one. This function exists to ease working with timespans of different types.
sub :: (HasDaysPrism s, HasDaysIso t) => t -> s -> t
sub = flip $ \s -> daysIso %~ flip subtract (s ^. toDays)

-- | Multiplicate a timespan with a number, which means repeating the timespan n times.
mult :: (HasDaysPrism a, Additive.C a, ToInteger.C b) => a -> b -> a
mult a b = sum $ replicate (fromInteger $ toInteger b) a

-- | Calculate the difference between to dates in days
difference :: Date -> Date -> Days
difference d e = e ^. daysFromOrigin - d ^. daysFromOrigin

-- | Calculate the least common timespan of two timespans.
lcs :: (HasDaysPrism a, HasDaysPrism b) => a -> b -> a
lcs a b = (^?! days . daysPrism) $ lcm (a ^. re daysPrism . from days) (b ^. toDays . from days)
