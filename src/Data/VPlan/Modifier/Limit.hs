{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Limit a modifier with some predicate on the index
module Data.VPlan.Modifier.Limit (
    Limit()
  , condition
  , bound
  , limited
  , lower
  , equal
  , greater
  ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Data
import           Data.Foldable       (Foldable (..))
import qualified Data.VPlan.At       as A
import           Data.VPlan.Class
import           Data.VPlan.TH
import           GHC.Generics

-- | The 'Limit' modifier takes another modifier and behaves like that one, but only when the index compared to
-- the bound gives the 'condition'. If that's not the case, it behaves like the empty modifier.
data Limit s i v = Limit { _condition :: Ordering, _bound :: i, _limited :: s i v } deriving (Generic, Eq)
makeLenses ''Limit
makeModifier ''Limit
derivePeriodic ''Limit

instance (Limited (s i v), Index (s i v) ~ i, Ord i, Enum i) => Limited (Limit s i v) where
  imin (Limit GT b u) = (max (succ b) <$> imin u) <|> (Just $ succ b)
  imin (Limit EQ b _) = Just b
  imin (Limit LT _ u) = imin u
  imax (Limit GT _ u) = imax u
  imax (Limit EQ b _) = Just b
  imax (Limit LT b u) = (min (pred b) <$> imax u) <|> (Just $ pred b)

deriving instance (Show i, Show (s i v)) => Show (Limit s i v)
deriving instance (Data i, Typeable2 s, Typeable i, Typeable v, Data (s i v)) => Data (Limit s i v)

instance (A.Contains f (s i v), Ord i, i ~ Index (s i v), Gettable f) => A.Contains f (Limit s i v) where
  contains i f l
    | compare i (l ^. bound) == l ^. condition = limited (A.contains i f) l
    | otherwise = coerce $ indexed f i False

instance (A.Ixed f (s i v), Applicative f, Ord i, i ~ Index (s i v)) => A.Ixed f (Limit s i v) where
  ix i f l
    | compare i (l ^. bound) == l ^. condition = limited (A.ix i f) l
    | otherwise = pure l

instance Functor (s i) => Functor (Limit s i) where fmap f = limited %~ fmap f
instance Bifunctor s => Bifunctor (Limit s)   where bimap f g (Limit c b u) = Limit c (f b) $ bimap f g u
instance Contravariant (s i) => Contravariant (Limit s i) where contramap f = limited %~ contramap f
instance Foldable (s i) => Foldable (Limit s i) where fold = fold . view limited
instance Traversable (s i) => Traversable (Limit s i) where traverse = limited . traverse

instance (FromJSON (s i v), FromJSON i) => FromJSON (Limit s i v) where
  parseJSON (Object o) = Limit <$> (fmap read $ o .: "condition") <*> o .: "bound" <*> o .: "child"
  parseJSON v = typeMismatch "Object" v

instance (ToJSON (s i v), ToJSON i) => ToJSON (Limit s i v) where
  toJSON (Limit c b m) = object [ "condition" .= show c, "bound" .= b, "child" .= m ]

-- | Limit another schedule to all indices lower than the given one
lower :: (Ord i) => i -> s i v -> Limit s i v
lower = Limit LT

-- | Pick only the value at the given index from anther schedule
equal :: (Ord i) => i -> s i v -> Limit s i v
equal = Limit EQ

-- | Limit another schedule to all the indices heigher than the given one
greater :: (Ord i) => i -> s i v -> Limit s i v
greater = Limit GT
