{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, ExistentialQuantification, FlexibleContexts #-}

module Core.Schedule where

import qualified Core.Query as Q
import Core.Timespan
import Control.Arrow
import Control.Monad

newtype Date = Date Int deriving (Num, Enum, Eq, Show, Read, Integral, Real, Ord) 

data TimeQuery t = forall a. (Q.Query a t Bool) => TimeQuery a
data Schedule t a = When (TimeQuery t) a | Chain (Schedule t a) (Schedule t a)


days :: Int -> DateTimespan
days = DateTimespan

weeks :: Int -> DateTimespan
weeks = days . (*7)

every' :: (Eq i, Integral i) => i -> a -> Schedule i a
every' x = When (TimeQuery $ Q.pack (Q.equal (pred x)) <<< Q.pack (Q.modBy x))

always' :: a -> Schedule i a
always' = When (TimeQuery $ Q.const True)

shift' :: (Num i) => i -> Schedule i a -> Schedule i a
shift' i (When (TimeQuery q) a) = When (TimeQuery $ Q.pack (Q.subtract i) >>> Q.pack q) a
shift' i (Chain a b) = Chain (shift' i a) (shift' i b)

at :: t -> Schedule t a -> [a]
at t (When (TimeQuery q) a) = guard (Q.run q t) >> return a
at t (Chain a b) = at t a ++ at t b