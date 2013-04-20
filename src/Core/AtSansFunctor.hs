{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
-- | This module is a workaround for a bug in GHC 7.4. We need the lens' classes
-- 'Contains', 'Ixed' and 'At' without their Functor super class. The names in this
-- module clash with lens, so you need to import it qualified.
module Core.AtSansFunctor
  ( Contains, contains,
    Ixed, ix,
    At, at
  ) where

import          Control.Lens (IndexedLensLike', Index, IxValue, IndexedLens')

class Contains f m where
  contains :: Index m -> IndexedLensLike' (Index m) f m Bool

class Ixed f m where
  ix :: Index m -> IndexedLensLike' (Index m) f m (IxValue m)

class At f m where
  at :: Index m -> IndexedLens' (Index m) m (Maybe (IxValue m))
