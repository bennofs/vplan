{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Some template-haskell functions for generating boilerplate instances and defintions.
module Data.VPlan.TH
  ( -- * Interface
    genIxedFamilies
  , genIxedInstances
  , makeModifier
  , Modifiers

    -- * Implementation
  , appsT
  , getTVName
  , getTCInfo
  , resolveType

    -- * Reexports
  , module Data.Typeable.TH
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import qualified Data.VPlan.At  as A
import           Data.Traversable    (for)
import           Language.Haskell.TH
import           Data.Typeable.TH

-- | Get the modifiers of a schedule.
type family Modifiers s :: (* -> * -> *) -> * -> * -> *

-- | Generate the type families IxValue and Index for lens.
genIxedFamilies :: Name -> Q [Dec]
genIxedFamilies = reify >=> getTCInfo >=> for [mkName "Index", mkName "IxValue"] . genIxedFamily

-- | Get the name of a type variable binder
getTVName :: TyVarBndr -> Name
getTVName (PlainTV a) = a
getTVName (KindedTV a _) = a

-- | Apply a type constructor on a list of types
appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT

-- | Get the schedule type of a modifier
getScheduleType :: [TyVarBndr] -> TypeQ
getScheduleType tv = vtvn (init $ init tv) `appT` vtvn (init tv) `appT` vtvn tv
  where vtvn = varT . getTVName . last

-- | Generate the ixed family f for the type t with the type arguments tv.
genIxedFamily :: (Name, [TyVarBndr]) -> Name -> Q Dec
genIxedFamily (t,tv) fam = tySynInstD fam [appsT (conT t) $ map (varT . getTVName) tv] $ (conT fam) `appT` getScheduleType tv

-- | Generate the ixed and contains instances from the AtSansFunctor.Ixed/Contains instances for type t.
genIxedInstancesT :: Type -> Q [Dec]
genIxedInstancesT t = let z = return t in
  [d|
   instance (Functor f, A.Contains f $z) => Contains f $z where
     contains = A.contains

   instance (Functor f, A.Ixed f $z) => Ixed f $z where
     ix = A.ix
  |]

-- | Get the name of a type constructor, together with it's type variable binders.
getTCInfo :: Info -> Q (Name, [TyVarBndr])
getTCInfo (TyConI (NewtypeD _ n tv _ _)) = return (n,tv)
getTCInfo (TyConI (DataD _ n tv _ _)) = return (n,tv)
getTCInfo _ = fail "Expected newtype or data type constructor"

-- | Same as genIxedInstancesT, but taking a Name instead of a Type.
genIxedInstances :: Name -> Q [Dec]
genIxedInstances = reify >=> getTCInfo >=> \(n, tv) -> genIxedInstancesT (foldl AppT (ConT n) $ map (VarT . getTVName) tv)

-- | Get the type to a given name
resolveType :: Name -> Q Type
resolveType = reify >=> getTCInfo >=> \(n, tv) -> foldl appT (conT n) $ map (varT . getTVName) tv

-- | Make a modifiers type instance
genModifier :: Name -> Q [Dec]
genModifier n = let t = resolveType n in
  [d|
    type instance Modifiers $t = Modifiers $kind2type
  |]
  where kind2type = reify n >>= getTCInfo >>= \(_,tv) -> (vtvn $ init $ init tv) `appT` (vtvn $ init tv) `appT` (vtvn tv)
        vtvn = varT . getTVName . last 

-- | Make all boilerplate instances required for a modifier
makeModifier :: Name -> Q [Dec]
makeModifier n = concat <$> traverse ($ n) [genIxedFamilies, genIxedInstances, genModifier, makeTypeable]
