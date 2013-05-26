{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : $Header$
-- Description : A repeating modifier.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (uses various GHC extensions)
module Data.VPlan.Modifier.Repeat
  ( Repeat(..)
  ) where

import           Control.Lens
import           Data.Data
import qualified Data.VPlan.At    as A
import           Data.VPlan.Class
import           Data.VPlan.TH

-- | The repeat modifier repeats another modifier after a given interval.
data Repeat s = Repeat
    { _rinterval :: Span (Index s)
    , _repeated  :: s
    }
makeLenses ''Repeat
makeModifier ''Repeat

deriving instance (Data (Span (Index s)), Data s) => Data (Repeat s)
deriving instance (Eq (Span (Index s)), Eq s) => Eq (Repeat s)
instance Periodic (Repeat s) where interval r = r ^. rinterval
instance Limited (Repeat s) where
  imin _ = Nothing
  imax _ = Nothing

instance (Functor f, A.Contains f s, HasSpan (Index s)) => A.Contains f (Repeat s) where
  contains i f r = repeated ?? r $ A.contains (i `tmod` (r ^. rinterval)) f

instance (Functor f, A.Ixed f s, HasSpan (Index s)) => A.Ixed f (Repeat s) where
  ix i f r = repeated ?? r $ A.ix (i `tmod` (r ^. rinterval)) f
