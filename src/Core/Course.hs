{-# LANGUAGE TypeFamilies,TypeOperators, GeneralizedNewtypeDeriving #-}
module Core.Course where

import Data.Has

newtype Name = Name String
newtype ShortName = ShortName String
newtype Room = Room Int
type instance TypeOf Name = Name
type instance TypeOf ShortName = ShortName

type Teacher = FieldOf Name :&: FieldOf ShortName
type Subject = FieldOf Name :&: FieldOf ShortName
type Group = FieldOf ShortName
data Course = Split [(Group, Course)] | Course Subject Teacher Room | Canceled | Free

