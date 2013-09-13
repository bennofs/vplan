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
    Limit(..)
  , condition
  , limit
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
data Limit s c i v = Limit { _condition :: Ordering, _bound :: i, _limited :: s c i v } deriving (Generic, Eq)
makeLenses ''Limit
makeModifier ''Limit

limit :: Ordering -> i -> s c i v -> Limit s c i v
limit = Limit

instance (Limited (s c i v), Index (s c i v) ~ i, Ord i, Enum i) => Limited (Limit s c i v) where
  imin (Limit GT b u) = (max (succ b) <$> imin u) <|> (Just $ succ b)
  imin (Limit EQ b _) = Just b
  imin (Limit LT _ u) = imin u
  imax (Limit GT _ u) = imax u
  imax (Limit EQ b _) = Just b
  imax (Limit LT b u) = (min (pred b) <$> imax u) <|> (Just $ pred b)

deriving instance (Show i, Show (s c i v)) => Show (Limit s c i v)
deriving instance (Read i, Read (s c i v)) => Read (Limit s c i v)
deriving instance (Data i, Typeable3 s, Typeable i, Typeable c, Typeable v, Data (s c i v)) => Data (Limit s c i v)

instance (A.Contains f (s c i v), Ord i, i ~ Index (s c i v), Gettable f) => A.Contains f (Limit s c i v) where
  contains i f l
    | compare i (l ^. bound) == l ^. condition = limited (A.contains i f) l
    | otherwise = coerce $ indexed f i False

instance (A.Ixed f (s c i v), Applicative f, Ord i, i ~ Index (s c i v)) => A.Ixed f (Limit s c i v) where
  ix i f l
    | compare i (l ^. bound) == l ^. condition = limited (A.ix i f) l
    | otherwise = pure l

instance Functor (s c i) => Functor (Limit s c i) where fmap f = limited %~ fmap f
instance Bifunctor (s c) => Bifunctor (Limit s c)  where bimap f g (Limit c b u) = Limit c (f b) $ bimap f g u
instance Contravariant (s c i) => Contravariant (Limit s c i) where contramap f = limited %~ contramap f
instance Foldable (s c i) => Foldable (Limit s c i) where foldMap = views limited . foldMap
instance Traversable (s c i) => Traversable (Limit s c i) where traverse = limited . traverse
instance Periodic (s c i v) => Periodic (Limit s c i v) where interval = views limited interval

instance (FromJSON (s c i v), FromJSON i) => FromJSON (Limit s c i v) where
  parseJSON (Object o) = Limit <$> (fmap read $ o .: "condition") <*> o .: "bound" <*> o .: "child"
  parseJSON v = typeMismatch "Object" v

instance (ToJSON (s c i v), ToJSON i) => ToJSON (Limit s c i v) where
  toJSON (Limit c b m) = object [ "condition" .= show c, "bound" .= b, "child" .= m ]

-- | Limit another schedule to all indices lower than the given one
lower :: (Ord i) => i -> s c i v -> Limit s c i v
lower = Limit LT

-- | Pick only the value at the given index from anther schedule
equal :: (Ord i) => i -> s c i v -> Limit s c i v
equal = Limit EQ

-- | Limit another schedule to all the indices heigher than the given one
greater :: (Ord i) => i -> s c i v -> Limit s c i v
greater = Limit GT
