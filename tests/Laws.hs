{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | General law tests for instances defined in this package
module Laws
  (
    -- * Lens laws
    isSetter
  , isTraversal
  , isIso
  , isPrism

    -- * Laws for classes
  , isFunctor
  , isProfunctor
  , isBifunctor
  , isContravariant
  , isIxed
  , isAeson
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy     as LBS
import           Data.Functor.Compose
import           Data.Maybe
import           Instances()
import           Test.QuickCheck
import           Test.QuickCheck.Function

-- Taken from properties.hs from the lens source code:
setter_id :: Eq s => Setter' s a -> s -> Bool
setter_id l s = over l id s == s

setter_composition :: Eq s => Setter' s a -> s -> Fun a a -> Fun a a -> Bool
setter_composition l s (Fun _ f) (Fun _ g) = over l f (over l g s) == over l (f . g) s

lens_set_view :: Eq s => Lens' s a -> s -> Bool
lens_set_view l s = set l (view l s) s == s

lens_view_set :: Eq a => Lens' s a -> s -> a -> Bool
lens_view_set l s a = view l (set l a s) == a

setter_set_set :: Eq s => Setter' s a -> s -> a -> a -> Bool
setter_set_set l s a b = set l b (set l a s) == set l b s

iso_hither :: Eq s => Simple AnIso s a -> s -> Bool
iso_hither l s = s ^.cloneIso l.from l == s

iso_yon :: Eq a => Simple AnIso s a -> a -> Bool
iso_yon l a = a^.from l.cloneIso l == a

prism_yin :: Eq a => Prism' s a -> a -> Bool
prism_yin l a = preview l (review l a) == Just a

prism_yang :: Eq s => Prism' s a -> s -> Bool
prism_yang l s = maybe s (review l) (preview l s) == s

traverse_pure :: forall f s a. (Applicative f, Eq (f s)) => LensLike' f s a -> s -> Bool
traverse_pure l s = l pure s == (pure s :: f s)

traverse_pureMaybe :: Eq s => LensLike' Maybe s a -> s -> Bool
traverse_pureMaybe = traverse_pure

traverse_pureList :: Eq s => LensLike' [] s a -> s -> Bool
traverse_pureList = traverse_pure

traverse_compose :: (Applicative f, Applicative g, Eq (f (g s)))
                    => Traversal' s a -> (a -> g a) -> (a -> f a) -> s -> Bool
traverse_compose t f g s = (fmap (t f) . t g) s == (getCompose . t (Compose . fmap f . g)) s

isSetter :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a)
         => Setter' s a -> Property
isSetter l = setter_id l .&. setter_composition l .&. setter_set_set l

isTraversal :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Function a)
         => Traversal' s a -> Property
isTraversal l = isSetter l .&. traverse_pureMaybe l .&. traverse_pureList l
                 .&. do t <- arbitrary
                        (Fun _ leftOrRight) <- arbitrary
                        property $ traverse_compose l (\x -> if leftOrRight x then Left (show x) else Right x)
                                                      (\x -> if t then Just x else Nothing)

isLens :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a)
       => Lens' s a -> Property
isLens l = lens_set_view l .&. lens_view_set l .&. isTraversal l

isIso :: (Arbitrary s, Arbitrary a, CoArbitrary s, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function s, Function a)
      => Iso' s a -> Property
isIso l = iso_hither l .&. iso_yon l .&. isLens l .&. isLens (from l)

isPrism :: (Arbitrary s, Arbitrary a, CoArbitrary a, Show s, Show a, Eq s, Eq a, Function a)
      => Prism' s a -> Property
isPrism l = isTraversal l .&. prism_yin l .&. prism_yang l

isFunctor :: forall f a proxy. (Functor f, Arbitrary (f a), Eq (f a), Show (f a)) => proxy (f a) -> Property
isFunctor _ = property $ \f -> fmap id f == (f :: f a)

isProfunctor :: forall p a b proxy. (Profunctor p, Arbitrary (p a b), Eq (p a b), Show (p a b)) => proxy (p a b) -> Property
isProfunctor _ = property $ \p -> dimap id id p == (p :: p a b) && lmap id p == p && rmap id p == p

isContravariant :: forall f a proxy. (Contravariant f, Arbitrary (f a), Eq (f a), Show (f a)) => proxy (f a) -> Property
isContravariant _ = property $ \f -> contramap id f == (f :: f a)

isBifunctor :: forall f a b proxy. (Bifunctor f, Arbitrary (f a b), Eq (f a b), Show (f a b)) => proxy (f a b) -> Property
isBifunctor _ = property $ \f -> bimap id id f == (f :: f a b) && first id f == f && second id f == f

-- Assumes Gettable-only contains
isIxed :: forall proxy a. (Eq a, CoArbitrary (IxValue a), Function (IxValue a), Arbitrary (IxValue a), Show (IxValue a), Arbitrary a, Show a, Arbitrary (Index a), Show (Index a), Ixed (Bazaar (->) (IxValue a) (IxValue a)) a, Contains (Accessor Bool) a) => proxy a -> Property
isIxed _ = property $ \i ->
  let l = ix i :: LensLike' (Bazaar (->) (IxValue a) (IxValue a)) a (IxValue a)
  in  isTraversal (cloneTraversal l)
  .&. property (\(s :: a) -> isJust (s ^? cloneTraversal (ix i)) == s ^. contains i)

isAeson :: forall proxy a. (Function a, FromJSON a, Eq a, Show a, ToJSON a, CoArbitrary a, Arbitrary a) => proxy a -> Property
isAeson _ = isTraversal encodeDecode .&. prism_yin encodeDecode
  where encodeDecode = prism' encode decode :: Prism' LBS.ByteString a
