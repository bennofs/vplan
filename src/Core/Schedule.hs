{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Module      : $Header$
-- Description : The schedule data type.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (uses various GHC extensions)
module Core.Schedule (
    Schedule(..)
  , schedule
  ) where

import           Control.Lens
import qualified Core.AtSansFunctor as A

-- | The type of a schedule. This type is just fix at the type level, but keeping the
-- schedule info.
newtype Schedule i v s = Schedule (s (Schedule i v s))
makeIso ''Schedule

type instance Index (Schedule i v s) = i
type instance IxValue (Schedule i v s) = v

deriving instance (A.Contains f (s (Schedule i v s))) => A.Contains f (Schedule i v s)
deriving instance (A.Ixed f (s (Schedule i v s)), IxValue (s (Schedule i v s)) ~ v,
                   Index (s (Schedule i v s)) ~ i) => A.Ixed f (Schedule i v s)
instance (A.Ixed f (Schedule i v s), Functor f) => Ixed f (Schedule i v s) where
  ix = A.ix

instance (A.Contains f (Schedule i v s), Functor f) => Contains f (Schedule i v s) where
  contains = A.contains

deriving instance (Eq (s (Schedule i v s))) => Eq (Schedule i v s)
