{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, ExistentialQuantification, UndecidableInstances, FunctionalDependencies #-}

module Core.Query (
  Query, run, pack, const,
  FunctionQuery(), GenericQuery(), ChainedQuery(), IdQuery(), ConstQuery(),
  And(), Or(), Not(),
  (&&), (||), not, (<), (>), (==),
  Less(), Equal(), Greater(), 
  Add(), Subtract(), Multiply(), Divide(), Mod(), Quot(),
--  (+), (-), (*), (/), mod, quot, rem, div,
  ) where

import Control.Arrow
import Prelude hiding ((&&), (||), not, const, (.), id, subtract, (<), (>), (==), mod, quot, rem, div, (+), (-),(*),(/))
import Control.Category
import qualified Prelude as P

class Query q i o | q -> i, q -> o where
  run :: q -> i -> o

---- General queries
data IdQuery = IdQuery
instance Query IdQuery a a where
  run _ = id

data GenericQuery i o = forall q. (Query q i o) => GenericQuery q

class QPack q i o where
  pack :: q -> GenericQuery i o
  
instance QPack (GenericQuery i o) i o where
  pack = id
  
instance (Query q i o) => QPack q i o where
  pack = GenericQuery

instance Query (GenericQuery i o) i o where
  run (GenericQuery q) = run q
  
newtype FunctionQuery i o = FunctionQuery (i -> o)  
instance Query (FunctionQuery i o) i o where
  run ~(FunctionQuery f) = f

newtype ChainedQuery i t o = ChainedQuery (GenericQuery i t, GenericQuery t o)
instance Query (ChainedQuery i t o) i o where
  run ~(ChainedQuery ~(a,b)) q = run b $ run a q

instance Category GenericQuery where
  id = pack IdQuery
  a . b = pack $ ChainedQuery (b,a)

instance Arrow GenericQuery where
  arr = pack . FunctionQuery
  first a = pack $ FunctionQuery $ first $ run a
  
newtype ConstQuery a = ConstQuery a
const :: a -> ConstQuery a
const = ConstQuery
instance Query (ConstQuery a) i a where
  run ~(ConstQuery a) = P.const a

---- Logic queries -------------------------------------------------------------
newtype And a b = And (a,b)
newtype Or a b = Or (a,b)
newtype Not a = Not a
  
(&&) :: (Query a i Bool, Query b i Bool) => a -> b -> And a b
(||) :: (Query a i Bool, Query b i Bool) => a -> b -> Or a b
not :: (Query a i Bool) => a -> Not a
(&&) = curry And
(||) = curry Or
not = Not


instance (Query a i Bool, Query b i Bool) => Query (And a b) i Bool where
  run ~(And ~(a,b)) q = run a q P.&& run b q
  
instance (Query a i Bool, Query b i Bool) => Query (Or a b) i Bool where
  run ~(Or ~(a,b)) q = run a q P.|| run b q
  
instance (Query a i Bool) => Query (Not a) i Bool where
  run ~(Not a) q = P.not $ run a q 
  
---- Comparisation queries -----------------------------------------------------------
newtype Less a b = Less (a,b)
newtype Equal a b = Equal (a,b)
newtype Greater a b = Greater (a,b)
(<) :: (Ord a, Query q1 i a, Query q2 i a) => q1 -> q2 -> Less q1 q2
(==) :: (Eq a, Query q1 i a, Query q2 i a) => q1 -> q2 -> Equal q1 q2
(>) :: (Ord a, Query q1 i a, Query q2 i a) => q1 -> q2 -> Greater q1 q2
(<) = curry Less
(==) = curry Equal
(>) = curry Greater

instance (Ord a, Query q1 i a, Query q2 i a) => Query (Less q1 q2) i Bool where
  run ~(Less ~(a,b)) q = run a q P.< run b q
  
instance (Ord a, Query q1 i a, Query q2 i a) => Query (Greater q1 q2) i Bool where
  run ~(Greater ~(a,b)) q = run a q P.> run b q
  
instance (Eq a, Query q1 i a, Query q2 i a) => Query (Equal q1 q2) i Bool where
  run ~(Equal ~(a,b)) q = run a q P.== run b q

---- Arithmetic queries --------------------------------------------------------
newtype Add a b = Plus (a,b)
newtype Subtract a b = Subtract (a,b)
newtype Multiply a b = Multiply (a,b )
newtype Divide a b = Divide (a,b)
newtype Mod a b =  Mod (a,b)
newtype Quot a b = Quot (a,b)
newtype Div a b = Div (a,b)