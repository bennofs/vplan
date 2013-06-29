{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

-- | Functions to pretty-print schedules
module Data.VPlan.Print where

import           Control.Applicative
import           Control.Arrow            hiding ((<+>))
import           Control.Lens
import           Control.Monad.Identity
import           Control.Monad.Writer     hiding ((<>))
import           Data.Data
import           Data.Generics            hiding (gshow)
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
gshowQ :: (Data a) => Extender Box -> Bool -> a -> Box
gshowQ f p = runIdentity . tshow f' p
  where f' g = Identity . f (runIdentity . g)

-- | Like @gshowQ@, but allows to traverse with an applicative.
tshow :: (Data a, Applicative m) => (forall t. Typeable t => (t -> m Box) -> t -> m Box) -> Bool -> a -> m Box
tshow f p = showG `extQ` (pure . text) `ext1Q` slist
  where showG o = fmap ?? cs $ \cs' -> case cs' of
          [] -> c
          _ -> enc $ c <+> hsep 1 top cs'
          where c = text $ showConstr $ toConstr o
                cs = sequenceA $ gmapQ (f $ tshow f True) o
                enc = if p then enclose '(' ')' else id
        slist l = enclose '[' ']' . punctuateH top (char ',') <$> traverse (f $ tshow f True) (l `asTypeOf` [])

-- | Shows a value marking holes of a given type with "_" and returning an ordered list with the values of the holes.
showHoles :: forall p. forall a. (Data a, Typeable p) => Proxy p -> a -> (Box,[p])
showHoles _ a = runWriter $ (tshow (`extQ` f) False a :: Writer [p] Box)
  where f :: p -> Writer [p] Box
        f x = text "_" <$ tell [x]

-- | Show holes in a schedule, skipping the Enum/Schedule boiler plate.
showHolesSchedule :: forall i. forall v. forall s. (Data (Schedule i v s), EnumApply Data (s (Schedule i v s)), Typeable1 s, Typeable i, Typeable v) => Schedule i v s => (Box, [Schedule i v s])
showHolesSchedule s = enumApply (CFunc f :: CFunc Data (Box, [Schedule i v s])) $ review schedule s
  where f a = showHoles (Proxy :: Proxy (Schedule i v s)) a

-- | Show a tree
showTree :: Tree Box -> Box
showTree (Node n ns) = this <> connect // header // content
  where this = n <> text " "
        cs = map showTree ns
        intervals = map (succ . succ . cols) cs & _head %~ max (cols this)
        header = hcat top $ map (\x -> text "¦" <> rep (x-1) top (char ' ')) intervals
        connect = hcat top $ map (\x -> rep x top (char '-') <> char '+') $ safeInit $ intervals & _head %~ subtract (cols this)
        content = hcat top $ zipWith (\x y -> hpad y ' ' top x) cs intervals

-- | A version of 'init' that returns an empty list when given an empty list
safeInit :: [a] -> [a]
safeInit [] = []
safeInit x = init x

-- | Render a schedule as a tree.
showScheduleTree :: forall i. forall s. forall v. (EnumApply Data (s (Schedule i v s)), Typeable1 s, Typeable v, Typeable i, Data (s (Schedule i v s))) => Schedule i v s -> Box
showScheduleTree = showTree . unfoldTree showHolesSchedule


makeLenses ''ScheduleView

-- | Show a table
showTable :: 
