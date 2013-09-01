{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module provides functions for printing schedules.
module Data.VPlan.Print
  (-- * Printing functions
    showScheduleTree
  , showScheduleTable
   -- * Pretty printing utilities
  , enclose
  , rep
  , hpad
  , showFromToTable
  , showTree
   -- * Implementation
  , gshow
  , gshowQ
  , tshow
  , showHoles
  , showHolesSchedule
  , safeInit
   -- * Reexports
  , render
  , printBox
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Identity
import           Control.Monad.Writer     hiding ((<>))
import           Data.Data
import           Data.Generics            hiding (gshow)
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.Traversable         (sequenceA)
import           Data.Tree
import           Data.VPlan.Modifier.Enum
import           Data.VPlan.Schedule
import           Text.PrettyPrint.Boxes

-- | Type of a function that can extend a given pretty printing function that returns a type b.
type Extender b = (forall a. (Typeable a) => (a -> b) -> (a -> b))

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

-- | @gshow p a@ returns a string representation of a. If p is True, the result is enclosed in round parentheses.
gshow :: (Data a) => Bool -> a -> Box
gshow = gshowQ id

-- | Like @gshow@, but allows to specify a function that modifies the recursive caller function. This allows you
-- to provide custom special-case functions.
gshowQ :: forall a. (Data a) => Extender Box -> Bool -> a -> Box
gshowQ f p = runIdentity . tshow f' p
  where f' :: (forall t. Typeable t => (t -> Identity Box) -> t -> Identity Box)
        f' g = Identity . f (runIdentity . g)

-- | Like @gshowQ@, but allows to traverse with an applicative.
tshow :: forall m. forall a. (Data a, Applicative m) => (forall t. Typeable t => (t -> m Box) -> t -> m Box) -> Bool -> a -> m Box
tshow f p = showG `extQ` (pure . text) `ext1Q` slist
  where showG o = fmap ?? cs $ \cs' -> case cs' of
          [] -> c
          _ -> enc $ c <+> hsep 1 top cs'
          where c = text $ showConstr $ toConstr o
                cs = sequenceA $ gmapQ (f $ tshow f True) o
                enc = if p then enclose '(' ')' else id
        slist :: forall b. Data b => [b] -> m Box
        slist l = enclose '[' ']' . punctuateH top (char ',') <$> traverse (f $ tshow f True) (l `asTypeOf` [])

-- | Shows a value marking holes of a given type with "_" and returning an ordered list with the values of the holes.
showHoles :: forall p. forall a. (Data a, Typeable p) => Proxy p -> a -> (Box,[p])
showHoles _ a = runWriter $ (tshow (`extQ` f) False a :: Writer [p] Box)
  where f :: p -> Writer [p] Box
        f x = text "_" <$ tell [x]

-- | Show holes in a schedule, skipping the Enum/Schedule boiler plate.
showHolesSchedule :: forall s c i v. (Data (Schedule s c i v), EnumApply Data (s (Schedule s) c i v), Typeable3 (s (Schedule s)), Typeable c, Typeable i, Typeable v) => Schedule s c i v -> (Box, [Schedule s c i v])
showHolesSchedule s = enumApply (CFunc f :: CFunc Data (Box, [Schedule s c i v])) $ review schedule s
  where f :: forall a. (Data a) => a -> (Box, [Schedule s c i v])
        f a = showHoles (Proxy :: Proxy (Schedule s c i v)) a

-- | Show a tree
showTree :: Tree Box -> Box
showTree (Node n ns) = this <> connect // header // content
  where this = n <> text " "
        cs = map showTree ns
        intervals = map (succ . succ . cols) cs & _head %~ max (cols this)
        header = hcat top $ map (\x -> hpad x ' ' top $ text "¦") intervals
        connect = hcat top $ map (\x -> rep (pred x) top (char '-') <> char '+') $ safeInit $ intervals & _head %~ subtract (cols this - 1)
        content = hcat top $ zipWith (\x y -> hpad y ' ' top x) cs intervals

-- | A version of 'init' that returns an empty list when given an empty list
safeInit :: [a] -> [a]
safeInit [] = []
safeInit x = init x

-- | Render a schedule as a tree.
showScheduleTree :: forall s c i v. (EnumApply Data (s (Schedule s) c i v), Typeable3 (s (Schedule s)), Typeable3 (Schedule s), Typeable c, Typeable v, Typeable i, Data (s (Schedule s) c i v)) => Schedule s c i v -> Box
showScheduleTree = showTree . unfoldTree showHolesSchedule

-- | Show a function from a pair to a box as a table in the given range.
showFromToTable :: (Enum e) => ((e,e) -> Box) -> (e,e) -> (e,e) -> Box -> Box -> Box
showFromToTable f (x,y) (x',y') h v = hcat top $ intersperse h $ map g [x..x']
  where g a = vcat right $ intersperse v $ map ((v <>) . f . (a,)) [y..y']

-- | Render a part of a schedule as a table
showScheduleTable :: (Ixed (Accessor (First (IxValue s))) s, Index s ~ (a,a), Enum a, Show (IxValue s)) => (a,a) -> (a,a) -> s -> Box
showScheduleTable b e s = showFromToTable (\x -> text $ fromMaybe "-" $ fmap show $ s ^? ix x) b e (char ' ') nullBox
