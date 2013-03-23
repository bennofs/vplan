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

-- | The type of a schedule. This type is just fix at the type level, but with keeping the
-- schedule info.
newtype Schedule i v s = Schedule (s (Schedule i v s))
makeIso ''Schedule

type instance Index (Schedule i v s) = i
type instance IxValue (Schedule i v s) = v

deriving instance (Contains f (s (Schedule i v s))) => Contains f (Schedule i v s)
deriving instance (Ixed f (s (Schedule i v s)), IxValue (s (Schedule i v s)) ~ v,
                   Index (s (Schedule i v s)) ~ i) => Ixed f (Schedule i v s)
