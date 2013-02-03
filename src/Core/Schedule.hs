{-# LANGUAGE TemplateHaskell #-}
module Core.Schedule (skipRange, repeatEvery, validRange, val, at, Item, Schedule, Range, shiftStart, makeWeekSchedule, 
                      query, addSkipRange, (.?), shiftEnd, shift) where 

import Data.Monoid
import Core.Course
import Data.List
import Control.Lens hiding (at)

type Range d = (d,d)
data Item d a = Item { _skipRange :: [Range d], _repeatEvery :: d, _validRange :: (Range d), _val :: a}
makeLenses ''Item
type Schedule d a = [Item d a]

inRange :: (Ord d) => d -> Range d -> Bool
inRange x (s,e) = x >= s && x < e

rangeLength :: (Num d) => Range d -> d
rangeLength (x,y) = y - x

isInnerRangeOf :: (Ord d) => Range d -> Range d -> Bool
isInnerRangeOf (s1, e1) r = s1 `inRange` r || e1 `inRange` r

skip :: (Num d, Ord d) => d -> [Range d] -> d -> d
skip x es d
  | null es' = d + x
  | otherwise = skip (x + skipExtra es) (es \\ es') d
  where es' = filter (`isInnerRangeOf` (d, d + x)) es
        skipExtra = sum . map rangeLength

at :: (Num d, Ord d) => Item d a -> d -> Maybe a
at i d
  | not $ d `inRange` (i^.validRange) = Nothing
  | d == i^.validRange._1 = Just $ i^.val
  | otherwise = (`at` d) $ validRange._1 .~ next $ i
  where next = skip (i^.repeatEvery) (i^.skipRange) (i^.validRange._1)

shiftStart :: (Num d, Ord d) => d -> Item d a -> Item d a
shiftStart d i = i & validRange._1 %~ (skip d $ i^.skipRange)

shiftEnd :: (Num d, Ord d) => d -> Item d a -> Item d a
shiftEnd d i = i & validRange._2 %~ (skip d $ i^.skipRange)

shift :: (Num d, Ord d) => d -> Item d a -> Item d a
shift d i = i & validRange.both %~ (skip d $ i^.skipRange)

query :: (Item d a -> Maybe a) -> Schedule d a -> Maybe a
query = alaf First ((mconcat.) . map)

(.?) :: Schedule d a -> (Item d a -> Maybe a) -> Maybe a
(.?) = flip query

makeWeekSchedule :: Range Int -> [[Course]] -> Schedule Int [Course]
makeWeekSchedule v = zipWith shiftStart [0..] .  map (Item [] 7 v)

addSkipRange :: Range d -> Item d a -> Item d a
addSkipRange r = skipRange %~ (r:)