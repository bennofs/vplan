{-# LANGUAGE TypeFamilies,TypeOperators, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
module Core.Course where

import Control.Lens.TH

newtype Room = Room Int deriving (Show)
makeIso ''Room

data Teacher = Teacher { _teacherName :: String, _teacherShortName :: String } deriving (Show)
makeFields ''Teacher

data Subject = Subject { _subjectName :: String, _subjectShortName :: String } deriving (Show)
makeFields ''Subject

data Group = Group { _groupShortName :: String } deriving (Show)
makeFields ''Group

data Course = Split [(Group, Course)] | Course Subject Teacher Room deriving (Show)