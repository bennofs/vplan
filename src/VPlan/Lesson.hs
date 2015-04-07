{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.VPlan.Lesson where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Group
import Data.Semigroup
import Data.VPlan.Time

newtype LessonLocation = LessonLocation { getLessonLocation ::  (WeekDate, DiscreteTime) }
  deriving (Eq, Ord, Monoid, Semigroup, Group)

_LessonLocation :: Iso' LessonLocation (WeekDate, DiscreteTime)
_LessonLocation = iso getLessonLocation LessonLocation

lessonLocation :: Integer -> WeekDay -> Integer -> LessonLocation
lessonLocation w d l = LessonLocation (WeekDate w d, _DiscreteTime # l)

lessonNr :: Lens' LessonLocation Integer
lessonNr = _LessonLocation . _2 . _DiscreteTime

lessonDate :: Lens' LessonLocation WeekDate
lessonDate = _LessonLocation . _1

instance HasWeek LessonLocation Integer where week = lessonDate . week
instance HasDay  LessonLocation WeekDay where day  = lessonDate . day

instance FromJSON LessonLocation where
  parseJSON = withObject "Object" $ \o -> fmap LessonLocation $ (,) <$> parseJSON (Object $ sans "nr" o) <*> (review _DiscreteTime <$> o .: "nr")

instance ToJSON LessonLocation where
  toJSON (LessonLocation (date, nr)) = let (Object o) = toJSON date in Object $ o & at "nr" ?~ Number (fromInteger $ nr ^. _DiscreteTime)

data Subject = Subject
  { _subjectShortName :: String
  , _subjectName :: String
  }
makeFields ''Subject

data Teacher = Teacher
  { _teacherShortName :: String
  , _teacherName :: String
  }
makeFields ''Teacher

data Lesson = Lesson
  { _subject :: Subject
  , _room :: Integer
  ,  _teacher :: Teacher
  }
makeLenses ''Lesson
