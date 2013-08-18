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

    -- * Functor/Profunctor/Bifunctor/Contravariant laws
  , isFunctor
  , isProfunctor
  , isBifunctor
  , isContravariant
  , isIxed
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Bifunctor
import           Data.Functor.Compose
import           Data.Maybe
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
                  .&. do as <- arbitrary
                         bs <- arbitrary
                         t <- arbitrary
                         property $ traverse_compose l (\x -> as++[x]++bs)
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

isFunctor :: forall f proxy. (Functor f, Arbitrary (f Int), Eq (f Int), Show (f Int)) => proxy f -> Property
isFunctor _ = property $ \f -> fmap id f == (f :: f Int)

isProfunctor :: forall p proxy. (Profunctor p, Arbitrary (p Int Int), Eq (p Int Int), Show (p Int Int)) => proxy p -> Property
isProfunctor _ = property $ \p -> dimap id id p == (p :: p Int Int) && lmap id p == p && rmap id p == p

isContravariant :: forall f proxy. (Contravariant f, Arbitrary (f Int), Eq (f Int), Show (f Int)) => proxy f -> Property
isContravariant _ = property $ \f -> contramap id f == (f :: f Int)

isBifunctor :: forall f proxy. (Bifunctor f, Arbitrary (f Int Int), Eq (f Int Int), Show (f Int Int)) => proxy f -> Property
isBifunctor _ = property $ \f -> bimap id id f == (f :: f Int Int) && first id f == f && second id f == f

-- Assumes Gettable-only contains
isIxed :: forall proxy a. (Eq a, CoArbitrary (IxValue a), Function (IxValue a), Arbitrary (IxValue a), Show (IxValue a), Arbitrary a, Show a, Arbitrary (Index a), Show (Index a), Ixed (Bazaar (->) (IxValue a) (IxValue a)) a, Contains (Accessor Bool) a) => proxy a -> Property
isIxed _ = property $ \i ->
  let l = ix i :: LensLike' (Bazaar (->) (IxValue a) (IxValue a)) a (IxValue a)
  in  isTraversal (cloneTraversal l)
  .&. property (\(s :: a) -> isJust (s ^? cloneTraversal (ix i)) == s ^. contains i)
