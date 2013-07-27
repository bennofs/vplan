{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Miscellaneous functions and types that belong to no other module
module Data.VPlan.Util
  ( (:$)
  , Flip
  , By
  ) where

-- | This is '($)' at the type level.
type a :$ b = a b
infixr 0 :$

-- | This is 'flip' at the type level.
type Flip a b c = a c b

-- | Alias for 'Flip'.
type By a b c = a c b
