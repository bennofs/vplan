{-# LANGUAGE TemplateHaskell #-}
module VPlan.Source.Manos where

import           Control.Lens
import qualified Data.Text as T

data Entry = Entry 
  { _course      :: T.Text
  , _lessonNr    :: Int
  , _teacherName :: T.Text
  , _room        :: Int
  , _info        :: T.Text
  }
makeLenses ''Entry

data Table = Table [Entry]



