module Core.Error (
  GenError(..), IOErr(..), NetworkErr(..), ParseErr(..), FileErr(..), GtkErr(..), ResponseErr(..), GenErrorT, 
  withSource, tryMaybe, tryEither, fileE, parseE, networkE, responseE, gtkE, gErrorMessage, gErrorCode, gErrorDomain, _GError, 
  check, checkM, (?->), (<-?), (=-?), (?-=),
  module Control.Error, module Control.Lens
  ) where

import Network.Stream
import Network.HTTP.Headers
import Network.URI
import Network.HTTP.Base
import Control.Error
import Control.Monad
import Control.Proxy
import Control.Applicative
import Control.Lens
import Control.Exception
import Control.Exception.Lens
import System.Glib.GError
import Control.Monad.CatchIO as E

data GenError = IoE {source :: URI, ioErr :: IOErr} deriving (Show)
data IOErr = NetworkE NetworkErr | ParseE ParseErr | GtkE GtkErr | FileE FileErr deriving (Show)
data FileErr = NotFoundE deriving (Show)
data GtkErr = GTKInitE deriving (Show)
data NetworkErr = ConnectionE ConnError | RequestE ResponseCode | ResponseE ResponseErr | URLInvalidE | ConnOpenE String deriving (Show)
data ResponseErr = MissingHeader HeaderName deriving (Show)
data ParseErr = PDFParseE deriving (Show)
type GenErrorT m a = EitherT GenError m a

withSource :: (Monad m) => URI -> EitherT IOErr m a -> EitherT GenError m a
withSource = fmapLT . IoE

networkE :: NetworkErr -> IOErr
networkE = NetworkE

parseE :: ParseErr -> IOErr
parseE = ParseE

fileE :: FileErr -> IOErr
fileE = FileE

responseE :: ResponseErr -> NetworkErr
responseE = ResponseE

gtkE :: GtkErr -> IOErr
gtkE = GtkE

tryMaybe :: (MonadCatchIO m) => Getting (First a) SomeException t a b -> m r -> MaybeT m r
tryMaybe p = hushT . tryEither p

tryEither :: (MonadCatchIO m) => Getting (First a) SomeException t a b -> m r -> EitherT a m r
tryEither p = EitherT . trying p

check :: (Monad m, Functor m) => Bool -> MaybeT m ()
check = (() <$) . guard

checkM :: (Monad m, Functor m) => MaybeT m Bool -> MaybeT m ()
checkM = (() <$) . (>>= guard)

_GError :: Prism' SomeException GError
_GError = prism' toException fromException

gErrorDomain :: Lens' GError GErrorDomain
gErrorDomain = lens (\(GError x _ _) -> x) $ \(GError _ y z) x -> GError x y z

gErrorCode :: Lens' GError GErrorCode
gErrorCode = lens (\(GError _ x _) -> x) $ \(GError x _ z) y -> GError x y z

gErrorMessage :: Lens' GError GErrorMessage
gErrorMessage = lens (\(GError _ _ x) -> x) $ \(GError x y _) z -> GError x y z

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