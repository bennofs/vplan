{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-} 
{-# LANGUAGE DeriveGeneric         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- | A modifier that can contain exactly one out of multiple modifiers of different types.
module Data.VPlan.Modifier.Enum (
    (:><:)(R,L)
  , enumSchedule
  , enumItem
  , scheduleItem
  , EnumContains(enumValue, castEnum)
  , EnumApply(enumApply)
  , CFunc(..)
  , enumv
  , BothInstance
  , BothInstance1
  , BothInstance2
  , C
  ) where

import           Control.Applicative
import           Data.Aeson.Types
import           Data.Maybe
import           GHC.Exts
import           Data.Foldable (Foldable(..))
import           Control.Lens
import           Control.Monad
import           Data.Data
import qualified Data.VPlan.At       as A
import           Data.VPlan.Builder
import           Data.VPlan.Class
import qualified Data.Text           as T
import           Data.VPlan.Schedule
import           Data.VPlan.TH
import           Data.VPlan.Util
import           Control.Lens.Aeson
import           GHC.Generics hiding (C)

-- | An Either for types with one type argument (which is passed to both sides)
data (:><:) a b (s :: * -> * -> *) i v = L (a s i v) | R (b s i v) deriving (Eq, Generic)
infixr 7 :><:

makeModifier ''(:><:)

deriving instance (BothInstance Show a b s i v) => Show (C a b s i v)
deriving instance (BothInstance Read a b s i v) => Read (C a b s i v)
deriving instance (Typeable v, Typeable i, BothInstance Data a b s i v, Typeable2 (C a b s)) => Data ((:><:) a b s i v)

-- | Shorter alias
type C = (:><:)

-- | Require that a type enum can contain the given value
class EnumContains a b where

  -- | Create an enum with the given value.
  enumValue :: a s i v -> b (s :: * -> * -> *) i v

  -- | Try to access the value of the requested type in the enum. Returns Nothing if a value of another type
  -- is currently stored in the enum.
  castEnum :: b (s :: * -> * -> *) i v -> Maybe :$ a s i v

instance EnumContains a a where 
  enumValue = id
  castEnum = Just

instance EnumContains a (C a b) where 
  enumValue = L
  castEnum = preview $ enumEither . _Left

instance EnumContains b (C a b) where 
  enumValue = R
  castEnum = preview $ enumEither . _Right

instance (EnumContains c b) => EnumContains c (C a b) where 
  enumValue = R . enumValue
  castEnum = preview (enumEither . _Right) >=> castEnum

type BothInstance (c :: * -> Constraint) a b (s :: * -> * -> *) i v = (c (a s i v), c (b s i v))
type BothInstance1 (c :: (* -> *) -> Constraint) a b (s :: * -> * -> *) i = (c (a s i), c (b s i))
type BothInstance2 (c :: (* -> * -> *) -> Constraint) a b (s:: * -> * -> *) = (c (a s), c (b s))
type BothSame f a b s i v = (f (C a b (s :: * -> * -> *) i v) ~ f (a s i v), f (C a b s i v) ~ f (b s i v))

instance (BothInstance (A.Contains f) a b s i v, BothSame Index a b s i v, Functor f) => A.Contains f (C a b s i v) where
  contains i f (L x) = L <$> A.contains i f x
  contains i f (R x) = R <$> A.contains i f x

instance (Functor f, BothInstance (A.Ixed f) a b s i v, BothSame Index a b s i v, BothSame IxValue a b s i v) => A.Ixed f (C a b s i v) where
  ix i f (L x) = L <$> A.ix i f x
  ix i f (R x) = R <$> A.ix i f x

instance (BothInstance Periodic a b s i v, BothSame Index a b s i v) => Periodic (C a b s i v) where
  interval (L a) = interval a
  interval (R a) = interval a

instance (BothInstance Limited a b s i v, BothSame Index a b s i v) => Limited (C a b s i v) where
  imin (L a) = imin a
  imin (R a) = imin a
  imax (L a) = imax a
  imax (R a) = imax a

instance (BothInstance1 Functor a b s i) => Functor (C a b s i) where
  fmap f (L x) = L $ fmap f x
  fmap f (R x) = R $ fmap f x

instance (BothInstance2 Bifunctor a b s) => Bifunctor (C a b s) where
  bimap f g (L x) = L $ bimap f g x
  bimap f g (R x) = R $ bimap f g x

instance (BothInstance2 Profunctor a b s) => Profunctor (C a b s) where
  dimap l r (L x) = L $ dimap l r x
  dimap l r (R x) = R $ dimap l r x

instance (BothInstance1 Contravariant a b s i) => Contravariant (C a b s i) where
  contramap f (L x) = L $ contramap f x
  contramap f (R x) = R $ contramap f x

instance (BothInstance1 Foldable a b s i) => Foldable (C a b s i) where
  foldMap f (L x) = foldMap f x
  foldMap f (R x) = foldMap f x

instance (BothInstance1 Traversable a b s i) => Traversable (C a b s i) where
  traverse f (L x) = L <$> traverse f x
  traverse f (R x) = R <$> traverse f x

-- | Unwrap one constructor application in a TypeRep
unapply :: TypeRep -> TypeRep
unapply t = typeRepTyCon t `mkTyConApp` dropEnd 1 (typeRepArgs t)

-- | Select a the given element of an association list, failing with the default given.
caseOf :: (Eq k) => (k -> a) -> [(k,a)] -> k -> a
caseOf def assoc k = fromMaybe (def k) $ assoc ^? traversed . filtered ((==k) . fst) . _2

instance (BothInstance FromJSON a (C b c) s i v, Typeable2 (a s)) => FromJSON (C a (C b c) s i v) where
  parseJSON (Object o) 
    | o ^? ix "modifier" . _String == Just (T.pack $ show typL) = L <$> parseJSON (Object $ sans "modifier" o)
    | otherwise = R <$> parseJSON (Object o)
    where typL = unapply $ typeOf2 (undefined :: a s () ())
  parseJSON v = typeMismatch "Object" v

instance (BothInstance FromJSON a b s i v, BothInstance2 Typeable2 a b s) => FromJSON (C a b s i v) where
  parseJSON (Object o) = o .: "modifier" >>= caseOf failure 
      [ (show typL, L <$> parseJSON (Object $ sans "modifier" o))
      , (show typR, R <$> parseJSON (Object $ sans "modifier" o))
      ]
    where failure m = fail $ "Unknown modifier: " ++ m
          typL = unapply $ typeOf2 (undefined :: a s () ())
          typR = unapply $ typeOf2 (undefined :: b s () ())
  parseJSON v = typeMismatch "Object" v

instance (BothInstance ToJSON a (C b c) s i v, Typeable2 (a s)) => ToJSON (C a (C b c) s i v) where
  toJSON (L x) = let (Object o) = toJSON x in Object $ o & at "modifier" ?~ String (T.pack $ show $ unapply $ typeOf2 (undefined :: a s () ()))
  toJSON (R x) = toJSON x

instance (BothInstance ToJSON a b s i v, BothInstance2 Typeable2 a b s) => ToJSON (C a b s i v) where
  toJSON (L x) = let (Object o) = toJSON x in Object $ o & at "modifier" ?~ String (T.pack $ show $ unapply $ typeOf2 (undefined :: a s () ()))
  toJSON (R x) = let (Object o) = toJSON x in Object $ o & at "modifier" ?~ String (T.pack $ show $ unapply $ typeOf2 (undefined :: b s () ()))

-- | A polymorphic, constrained function returning some type r.
data CFunc ctx r = CFunc
  { cfunc :: (forall a. ctx a => a -> r)
  }

-- | Class to which allows to apply functions to the values in an Enum
class EnumApply ctx e where
  enumApply :: CFunc ctx b -> e -> b

instance (ctx (l s i v), EnumApply ctx (r s i v)) => EnumApply ctx (C l r s i v) where
  enumApply f (L a) = cfunc f a
  enumApply f (R a) = enumApply f a

instance (ctx a) => EnumApply ctx a where
  enumApply f a = cfunc f a

-- | Build a value as a schedule containing an enum.
enumSchedule :: (EnumContains a s) => a (Schedule s) i v -> Schedule s i v
enumSchedule = view schedule . enumValue

-- | Build an enum value as a single item.
enumItem :: (EnumContains a e) => a (Schedule s) i v -> Builder (e (Schedule s) i v) ()
enumItem = item . enumValue

-- | Build an enum value as a single schedule item.
scheduleItem :: (EnumContains a s) => a (Schedule s) i v -> Builder (Schedule s i v) ()
scheduleItem = item . enumSchedule

instance (EnumContains m s) => Supported m (Schedule s) where new = enumSchedule

-- | Convert an Enum to/from an Either
enumEither :: Iso (C a b s i v) (C a' b' s' i' v') (Either (a s i v) (b s i v)) (Either (a' s' i' v') (b' s' i' v'))
enumEither = iso ?? either L R $ \e -> case e of
  L x -> Left x
  R x -> Right x

-- | A prism that focuses the element at the given type in an enum. The v stands for "value", it's there because the name
-- enum is already taken by lens.
enumv :: (EnumApply Typeable (e s i v), EnumContains a e, EnumContains b e) => Prism (e s i v) (e s i v) (a s i v) (b s i v)
enumv = prism enumValue (\e -> maybe (Left e) Right $ castEnum e)
