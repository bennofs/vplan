{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
-- | Functions to pretty-print schedules
module Data.VPlan.Print where

import           Control.Lens
import           Data.Data
import           Data.Data.Lens
import           Data.Generics          hiding (gshow)
import           Data.VPlan.Schedule
import           Debug.Trace
import           Text.PrettyPrint.Boxes

-- | Enclose a box in the given characters.
enclose :: Char -> Char -> Box -> Box
enclose f l b = char f <> b <> char l

-- | A box that just repeats another box n times horizontally
rep :: Int -> Alignment -> Box -> Box
rep n a = hcat a . replicate n

-- | Pad a box to the given number of columns, using the supplied fill character, and cat the boxes with the given
-- alignment together.
hpad :: Int -> Char -> Alignment -> Box -> Box
hpad n c a b
  | n > cols b = b <> rep (n - cols b) a (char c)
  | otherwise = b

-- | A generic show, just printing the data constructor and it's arguments
gshow :: (Data a) => a -> Box
gshow = gshowQ id

-- | Generalized gshow, which also takes a function that extends the recursive call.
gshowQ :: (Data a) => (forall a. Typeable a => (a -> Box) -> a -> Box) -> a -> Box
gshowQ f = showG `extQ` text `ext1Q` slist
  where showG o
          | null cs = c
          | otherwise = enclose '(' ')' $ c <+> hsep 1 top cs
          where c = text $ showConstr $ toConstr o
                cs = gmapQ (f $ gshowQ f) o
        slist :: (Data a) => [a] -> Box
        slist l = enclose '[' ']' $ punctuateH top (char ',') $ map (f $ gshowQ f) l

-- | Just const with a restricted type, to aid the type inference
asAppliedTo :: (a -> b) -> a -> (a -> b)
asAppliedTo = const

-- | Render a plated as a tree-like structure, rendering everything that is not recursed into as a node.
treeSchedule :: (Plated a, Data a) => a -> Box
treeSchedule a = text "  " <> this // header // content
  where showHole = const $ text "_"
        cs = map treeSchedule $ a ^.. plate
        this = gshowQ (extQ ?? showHole `asAppliedTo` a) a
        intervals = map ((+2) . cols) cs & _head %~ max (cols this)
        header = hcat top $ map (\n -> text "    |" <> rep (n-3) top (char ' ')) intervals
        content = hcat top $ zipWith (\x n -> hpad (n + 2) ' ' top x) cs intervals
