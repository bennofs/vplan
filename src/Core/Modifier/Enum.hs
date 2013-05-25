{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : $Header$
-- Description : A modifier that can contain exactly one out of multiple modifiers of possible
--               different types.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : non-portable (uses various GHC-specific extensions)
module Core.Modifier.Enum (
    (:><:)(R,L)
  , Close(..)
  , enumValue
  , enumSchedule
  , enumItem
  , scheduleItem
  , MakeTypeEnum
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Core.AtSansFunctor  as A
import           Core.Builder
import           Core.Schedule
import           Core.TH
import           Data.Data
import           Data.Void

-- | An Either for types with one type argument (which is passed to both sides)
data (:><:) a b s = L (a s) | R (b s) deriving (Eq)
infixr 7 :><:

-- | Shorter alias
type C = (:><:)

makeModifier ''(:><:)

deriving instance (Typeable s, Typeable1 a, Typeable1 b, Data (b s), Data (a s)) => Data ((:><:) a b s)

-- | This type signalizes the end of a chain of (:><:)'s.
data Close a = Close Void deriving (Eq)
makeModifier ''Close

deriving instance (Data a) => Data (Close a)

instance A.Contains f (Close a) where contains _ _ (Close v) = absurd v
instance A.Ixed f (Close a) where ix _ _ (Close v) = absurd v

-- | Create a type enum with a given value.
class MakeTypeEnum a b where

  -- | Create an enum with the given value.
  enumValue :: a -> b

instance (s ~ s')               => MakeTypeEnum (a s') (C a b s) where enumValue = L
instance (MakeTypeEnum c (b s)) => MakeTypeEnum c      (C a b s) where enumValue = R . enumValue

instance (A.Contains f (a s), A.Contains f (b s), Index (a s) ~ Index s, Index (b s) ~ Index s,
          Functor f) => A.Contains f (C a b s) where
  contains i f (L x) = L <$> A.contains i f x
  contains i f (R x) = R <$> A.contains i f x

instance (A.Ixed f (a s), Functor f, A.Ixed f (b s), Index (a s) ~ Index s, Index (b s) ~ Index s,
          IxValue (a s) ~ IxValue s, IxValue (b s) ~ IxValue s) => A.Ixed f (C a b s) where
  ix i f (L x) = L <$> A.ix i f x
  ix i f (R x) = R <$> A.ix i f x


-- | Build a value as a schedule containing an enum.
enumSchedule :: (MakeTypeEnum a (s (Schedule i v s))) => a -> Schedule i v s
enumSchedule = view schedule . enumValue

-- | Build an enum value as a single item.
enumItem :: (MakeTypeEnum a e) => a -> Builder e ()
enumItem = item . enumValue

-- | Build an enum value as a single schedule item.
scheduleItem :: (MakeTypeEnum a (s (Schedule i v s))) => a -> Builder (Schedule i v s) ()
scheduleItem = item . enumSchedule
