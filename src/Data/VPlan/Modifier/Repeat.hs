{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | A repeating modifier.
module Data.VPlan.Modifier.Repeat
  ( Repeat(..)
  ) where

import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At    as A
import           Data.VPlan.Class
import           Data.VPlan.TH

-- | The repeat modifier repeats another modifier after a given interval.
data Repeat s i v = Repeat
    { _rinterval :: Span i
    , _repeated  :: s i v
    }
makeLenses ''Repeat
makeModifier ''Repeat

deriving instance (Data (Span i), Data (s i v), Typeable2 s, Typeable i, Typeable v) => Data (Repeat s i v)
deriving instance (Eq (Span i), Eq (s i v)) => Eq (Repeat s i v)

instance (Index (s i v) ~ i) => Periodic (Repeat s i v) where interval r = r ^. rinterval
instance Limited (Repeat s i v) where
  imin _ = Nothing
  imax _ = Nothing

instance (Functor f, A.Contains f (s i v), HasSpan i, Index (s i v) ~ i) => A.Contains f (Repeat s i v) where
  contains i f r = repeated ?? r $ A.contains (i `tmod` (r ^. rinterval)) f

instance (Functor f, A.Ixed f (s i v), HasSpan i, Index (s i v) ~ i) => A.Ixed f (Repeat s i v) where
  ix i f r = repeated ?? r $ A.ix (i `tmod` (r ^. rinterval)) f
