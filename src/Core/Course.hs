{-# LANGUAGE TypeFamilies,TypeOperators, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
-- |
-- Module      : $Header$
-- Description : Provides data type declarations and lenses for a course and associated quantities.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable
module Core.Course (
    Room
  , room
  , Teacher (..)
  , Subject (..)
  , Group (..)
  , name, shortName
  , Course (..)
  ) where

import Control.Lens.TH

-- | Data type for a room
newtype Room = Room Int deriving (Show)
makeIso ''Room

-- | Data type for a teacher (with short (abbreviation) and full name)
data Teacher = Teacher { _teacherName :: String, _teacherShortName :: String } deriving (Show)
makeFields ''Teacher

-- | A subject
data Subject = Subject { _subjectName :: String, _subjectShortName :: String } deriving (Show)
makeFields ''Subject

-- | A group is an identifier for a group of pupils that have the same lesson.
data Group = Group { _groupShortName :: String } deriving (Show)
makeFields ''Group

-- | A course is one lesson.
data Course =
    -- | A split course is when the class is split in groups and each of the groups
    -- have different courses.
  Split [(Group, Course)]
    -- | A plain course, with a teacher, room and subject.
  | Course Subject Teacher Room deriving (Show)
