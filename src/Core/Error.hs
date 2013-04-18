{-# OPTIONS_GHC -fno-warn-orphans #-}
module Core.Error (
  GenError(..), IOErr(..), NetworkErr(..), FileErr(..), ResponseErr(..), GenErrorT,
  withSource, tryMaybe, tryEither, fileE, networkE, responseE,
  check, checkM, (?->), (<-?), (=-?), (?-=),
  module Control.Error, module Control.Lens
  ) where

import Core.Resource
import Network.Stream
import Network.HTTP.Headers
import Network.HTTP.Base
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Lens
import Control.Exception
import Control.Exception.Lens
import Data.Monoid
import Control.Monad.CatchIO as E

data GenError = IoE {source :: Resource, ioErr :: IOErr}
data IOErr = NetworkE NetworkErr | FileE FileErr deriving (Show)
data FileErr = NotFoundE deriving (Show)
data NetworkErr = ConnectionE ConnError | RequestE ResponseCode | ResponseE ResponseErr | URLInvalidE | ConnOpenE String deriving (Show)
data ResponseErr = MissingHeader HeaderName deriving (Show)
type GenErrorT m a = EitherT GenError m a

withSource :: (Monad m) => Resource -> EitherT IOErr m a -> EitherT GenError m a
withSource = fmapLT . IoE

networkE :: NetworkErr -> IOErr
networkE = NetworkE

fileE :: FileErr -> IOErr
fileE = FileE

responseE :: ResponseErr -> NetworkErr
responseE = ResponseE

tryMaybe :: (MonadCatchIO m) => Getting (First a) SomeException a -> m r -> MaybeT m r
tryMaybe p = hushT . tryEither p

tryEither :: (MonadCatchIO m) => Getting (First a) SomeException a -> m r -> EitherT a m r
tryEither p = EitherT . trying p

check :: (Monad m, Functor m) => Bool -> MaybeT m ()
check = (() <$) . guard

checkM :: (Monad m, Functor m) => MaybeT m Bool -> MaybeT m ()
checkM = (() <$) . (>>= guard)

instance (E.MonadCatchIO m) => E.MonadCatchIO (EitherT e m) where
  block x = EitherT $ E.block (runEitherT x)
  unblock x = EitherT $ E.unblock (runEitherT x)
  x `catch` f = EitherT $ runEitherT x `E.catch` (runEitherT . f)

(?->) :: Monad m => MaybeT m a -> b -> EitherT b m a
(?->) = flip noteT
infixl 1 ?->

(<-?) :: Monad m => b -> MaybeT m a -> EitherT b m a
(<-?) = noteT
infixr 1 <-?

(?-=) :: Monad m => EitherT b m a -> (b -> c) -> EitherT c m a
(?-=) = flip fmapLT
infixl 1 ?-=

(=-?) :: Monad m => (b -> c) -> EitherT b m a -> EitherT c m a
(=-?) = fmapLT
infixr 1 =-?
