{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Core.Schedule (skipRange, repeatEvery, validRange, val, Item, Schedule, Range, iAt, iShiftStart, sAt, makeWeekSchedule) where

import Core.Course
import Control.Monad
import Data.Monoid
import Data.List
import Control.Lens

type Range d = (d,d)
data Item d a = Item { _skipRange :: [Range d], _repeatEvery :: d, _validRange :: (Range d), _val :: a}
makeLenses ''Item
type Schedule = [Item Int [Course]]

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

iAt :: (Num d, Ord d, MonadPlus m) => Item d a -> d -> m a
iAt i d
  | not $ d `inRange` (i^.validRange) = mzero
  | d == i^.validRange._1 = return $ i^.val
  | otherwise = flip iAt d $ i & validRange._1 .~ next
  where next = skip (i^.repeatEvery) (i^.skipRange) (i^.validRange._1)

iShiftStart :: (Num d) => d -> Item d a -> Item d a
iShiftStart = over (validRange._1) . (+)

sAt :: Schedule -> Int -> [Course]
sAt = flip $ \d -> maybe [] id . getFirst . mconcat . map (First . (`iAt` d))

makeWeekSchedule :: Range Int -> [[Course]] -> Schedule
makeWeekSchedule v = zipWith iShiftStart [0..] .  map (Item [] 7 v)