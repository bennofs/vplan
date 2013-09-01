{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
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
{-# LANGUAGE TypeOperators         #-}
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
import           Data.Group
import           Data.Monoid
import qualified Data.VPlan.At       as A
import           Data.VPlan.Class
import           Data.VPlan.TH
import           Data.VPlan.Util
import           GHC.Generics

-- | The repeat modifier repeats another modifier after a given interval. To use this modifier, @span@ should be equal to @Span i.@
-- They can get out of sync when using bimap or dimap.
data Repeat s c i v = Repeat
    { _rinterval' :: (i,i)  -- These need to be subtracted to get the interval. We store both to avoid requiring a Group constraint in Bifunctor.
    , _repeated   :: s c i v
    } deriving (Generic)
makeLenses ''Repeat
makeModifier ''Repeat

pairedDiff :: (Functor f, Group a) => (a -> f a) -> (a,a) -> f (a,a)
pairedDiff f (u,l) = g <$> f (u <> invert l)
  where g x = (x <> l,l)

rinterval :: (Group i) => Lens' (Repeat s c i v) i
rinterval = rinterval' . pairedDiff

deriving instance (Show i, Show (s c i v)) => Show (Repeat s c i v)
deriving instance (Read i, Read (s c i v)) => Read (Repeat s c i v)
deriving instance (Data i, Data (s c i v), Typeable3 s, Typeable c, Typeable i, Typeable v) => Data (Repeat s c i v)
instance (Group i, Eq i, Eq (s c i v)) => Eq (Repeat s c i v) where
  a == b = a ^. rinterval == b ^. rinterval
           && a ^. repeated == b ^. repeated

instance (Group i, Index (s c i v) ~ i) => Periodic (Repeat s c i v) where interval r = r ^. rinterval
instance Limited (Repeat s c i v) where
  imin _ = Nothing
  imax _ = Nothing

instance (Functor f, A.Contains f (s c i v), Group i, Ord i, Index (s c i v) ~ i) => A.Contains f (Repeat s c i v) where
  contains i f r = repeated ?? r $ A.contains (i `gmod` (r ^. rinterval)) f

instance (Functor f, A.Ixed f (s c i v), Ord i, Group i, Index (s c i v) ~ i) => A.Ixed f (Repeat s c i v) where
  ix i f r = repeated ?? r $ A.ix (i `gmod` (r ^. rinterval)) f

instance Functor (s c i) => Functor (Repeat s c i) where fmap f = repeated %~ fmap f
instance Bifunctor (s c) => Bifunctor (Repeat s c) where bimap f g (Repeat i u) = Repeat (bimap f f i) $ bimap f g u
instance Contravariant (s c i) => Contravariant (Repeat s c i) where contramap = over repeated . contramap
instance Foldable (s c i) => Foldable (Repeat s c i) where foldMap = views repeated . foldMap
instance Traversable (s c i) => Traversable (Repeat s c i) where traverse = repeated . traverse

instance (FromJSON i, Monoid i, FromJSON (s c i v)) => FromJSON (Repeat s c i v) where
  parseJSON (Object o) = Repeat . (,mempty) <$> o .: "interval" <*> o .: "child"
  parseJSON v = typeMismatch "Object" v

instance (ToJSON i, Group i, ToJSON (s c i v)) => ToJSON (Repeat s c i v) where
  toJSON (Repeat i c) = object [ "interval" .= (i ^. pairedDiff), "child" .= c ]
