{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : $Header$
-- Description : Some template-haskell functions for generating boilerplate instances and defintions.
-- Copyright   : (c) Benno Fünfstück
-- License     : GPL-3
--
-- Maintainer  : benno.fuenfstueck@gmail.com
-- Stability   : experimental
-- Portability : portable
module Data.VPlan.TH
  ( -- * Interface
    genIxedFamilies
  , genIxedInstances
  , genTypeable
  , makeModifier
  , Modifiers
    -- * Implementation
  , appsT
  , getTVName
  , getTCInfo
  , parseKind
  , parseKind'
  , resolveType
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import qualified Data.VPlan.At  as A
import           Data.Traversable    (for)
import           Language.Haskell.TH
import           Data.Typeable

-- | Get the modifiers of a schedule.
type family Modifiers s :: * -> *

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

-- | Generate the ixed family f for the type t with the type arguments tv.
genIxedFamily :: (Name, [TyVarBndr]) -> Name -> Q Dec
genIxedFamily (t,tv) f = tySynInstD f [appsT (conT t) $ map (varT . getTVName) tv] $
                         appT (conT f) $ varT $ getTVName $ last tv

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

data KindC = Chained [KindC] | Single | Partial KindC deriving (Eq)

parseKind :: Kind -> Q KindC
#if MIN_VERSION_template_haskell(2,8,0)
parseKind (AppT ArrowT x) = Partial <$> parseKind x
parseKind StarT = return Single
parseKind (AppT x y) =  parseKind x >>= \x' -> case x' of
  (Partial k) -> parseKind y >>= \y' -> case y' of
    (Partial _) -> fail "Invalid kind"
    Single -> return $ Chained [k,Single]
    Chained ks -> return $ Chained (k:ks)
  _ -> fail "Invalid kind"
parseKind _ = fail "Invalid kind"
#else
parseKind StarK = return Single
parseKind (ArrowK k1 k2) = parseKind k2 >>= \x -> case x of
  (Chained ks) -> Chained . (:ks) <$> parseKind k1
  Single -> Chained . (:[Single]) <$> parseKind k1
  _ -> fail "Invalid kind"
#endif

parseKind' :: TyVarBndr -> Q KindC
parseKind' (KindedTV _ k) = parseKind k
parseKind' _ = return Single

typeableOrder :: KindC -> Int
typeableOrder Single = 0
typeableOrder (Chained l) = length l - 1
typeableOrder (Partial _) = error "Invalid kind"

mtypeableOrder :: [KindC] -> Int
mtypeableOrder [] = 0
mtypeableOrder xs = length $ takeWhile (== Single) $ reverse xs

typeableInstanceName :: Int -> Name
typeableInstanceName 0 = mkName "Typeable"
typeableInstanceName x = mkName $ "Typeable" ++ show x

typeableMethodName  :: Int -> Name
typeableMethodName 0 = mkName "typeOf"
typeableMethodName x = mkName $ "typeOf" ++ show x

makePred :: Name -> KindC -> Pred
makePred n k = ClassP (typeableInstanceName $ typeableOrder k) [VarT n]

makeTypeOf :: Name -> KindC -> Q Exp
makeTypeOf n k = [| $m (undefined :: $t) |]
  where m = varE $ typeableMethodName (typeableOrder k)
        t = appsT (varT n) ts
        ts = replicate (typeableOrder k) $ tupleT 0

makeTypeableTI :: (Name, [TyVarBndr]) -> Q Dec
makeTypeableTI (n,tv) = do
  x <- mtypeableOrder <$> traverse parseKind' tv
  let tv' = take (length tv - x) tv
  let ctx = zipWith (makePred . getTVName) tv <$> traverse parseKind' tv'
  loc <- location
  let p = loc_package loc
  let m = loc_module loc
  let s = nameBase n
  appList <- zipWith (makeTypeOf . getTVName) tv <$> traverse parseKind' tv'
  let b = [| mkTyCon3 p m s `mkTyConApp` $(listE appList) |]
  instanceD ctx (appT (conT $ typeableInstanceName x) $ appsT (conT n) $ map (varT . getTVName) tv') $ [
    funD (typeableMethodName x) $ [clause [wildP] (normalB b) []]
   ]


-- | Generate instances for Data.TypeableN
genTypeable :: Name -> Q [Dec]
genTypeable = reify >=> getTCInfo >=> fmap pure . makeTypeableTI

-- | Get the type to a given name
resolveType :: Name -> Q Type
resolveType = reify >=> getTCInfo >=> \(n, tv) -> foldl appT (conT n) $ map (varT . getTVName) tv

-- | Make a modifiers type instance
genModifier :: Name -> Q [Dec]
genModifier n = let t = resolveType n in
  [d|
    type instance Modifiers $t = Modifiers $kind2type
  |]
  where kind2type = reify n >>= getTCInfo >>= \(_,tv) -> varT $ getTVName $ last tv

-- | Make all boilerplate instances required for a modifier
makeModifier :: Name -> Q [Dec]
makeModifier n = concat <$> traverse ($ n) [genTypeable, genIxedFamilies, genIxedInstances, genModifier]
