{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | A repeating modifier.
module Data.VPlan.Modifier.Repeat
  ( Repeat(..)
  , rinterval
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

-- | The repeat modifier repeats another modifier after a given interval. To use this modifier, @span@ should be equal to @Span i.@
-- They can get out of sync when using bimap or dimap.
data Repeat s i v = Repeat
    { _rinterval' :: (i,i)  -- These need to be subtracted to get the interval. We store both avoid requiring a Num constraint in Bifunctor.
    , _repeated   :: s i v
    }
makeLenses ''Repeat
makeModifier ''Repeat

pairedDiff :: (Functor f, Num a) => (a -> f a) -> (a,a) -> f (a,a)
pairedDiff f (u,l) = g <$> f (u - l)
  where g x = (x + l,l)

rinterval :: (Num i) => Lens' (Repeat s i v)i
rinterval = rinterval' . pairedDiff

deriving instance (Data i, Data (s i v), Typeable2 s, Typeable i, Typeable v) => Data (Repeat s i v)
deriving instance (Eq i, Eq (s i v)) => Eq (Repeat s i v)

instance (Num i, Index (s i v) ~ i) => Periodic (Repeat s i v) where interval r = r ^. rinterval
instance Limited (Repeat s i v) where
  imin _ = Nothing
  imax _ = Nothing

saveMod :: (Num a, Eq a, Integral a) => a -> a -> a
saveMod a b = if b == 0 then 0 else a `mod` b

instance (Functor f, A.Contains f (s i v), Integral i, Index (s i v) ~ i) => A.Contains f (Repeat s i v) where
  contains i f r = repeated ?? r $ A.contains (i `saveMod` (r ^. rinterval)) f

instance (Functor f, A.Ixed f (s i v), Integral i, Index (s i v) ~ i) => A.Ixed f (Repeat s i v) where
  ix i f r = repeated ?? r $ A.ix (i `saveMod` (r ^. rinterval)) f

instance Functor (s i) => Functor (Repeat s i) where fmap f = repeated %~ fmap f
instance Bifunctor s => Bifunctor (Repeat s) where bimap f g (Repeat i u) = Repeat (bimap f f i) $ bimap f g u
instance Contravariant (s i) => Contravariant (Repeat s i) where contramap = over repeated . contramap
instance Foldable (s i) => Foldable (Repeat s i) where fold = fold . view repeated
instance Traversable (s i) => Traversable (Repeat s i) where traverse = repeated . traverse

instance (FromJSON i, Num i, FromJSON (s i v)) => FromJSON (Repeat s i v) where
  parseJSON (Object o) = Repeat . (,0) <$> o .: "interval" <*> o .: "child"
  parseJSON v = typeMismatch "Object" v

instance (ToJSON i, Num i, ToJSON (s i v)) => ToJSON (Repeat s i v) where
  toJSON (Repeat i c) = object [ "interval" .= (i ^. pairedDiff), "child" .= c ]
