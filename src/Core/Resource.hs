{-# LANGUAGE FlexibleContexts #-}
module Core.Resource where

import Core.Error
import Control.Applicative
import Control.Proxy
import Control.Proxy.Trans.Either
import Control.Exception.Lens
import System.IO.Error.Lens
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import System.IO
import Network.HTTP
import Network.URI
import Graphics.UI.Gtk.Poppler.Document (documentGetNPages, documentGetPage, documentNewFromFile)
import Graphics.UI.Gtk.Poppler.Page (pageGetText)
import Graphics.UI.Gtk.General.General (initGUI)
import System.Directory (canonicalizePath, getTemporaryDirectory, doesFileExist, removeFile)


fileURI :: FilePath -> URI
fileURI f = URI "file:" Nothing f "" ""

urlURI :: String -> URI
urlURI f = URI "url:" Nothing f "" "" 

runIdentityPE :: (Proxy p) => EitherP e (IdentityP p) a a' b b' m r -> EitherP e p a a' b b' m r 
runIdentityPE = EitherP . runIdentityP . runEitherP

toEitherP :: (Proxy p, Monad m, MonadTrans (p a a' b b')) => EitherT e m r -> EitherP e p a a' b b' m r
toEitherP = EitherP . lift . runEitherT

pdf2textF' :: (MonadCatchIO m, Functor m) => FilePath -> EitherT GenError m [String]
pdf2textF' f = withSource (fileURI f) $ do
  checkM (liftIO $ doesFileExist f) ?-> fileE NotFoundE
  _ <- tryMaybe _ErrorCall (liftIO initGUI) ?-> gtkE GTKInitE
  f' <- liftIO $ canonicalizePath f
  (Just doc) <- tryMaybe _GError (liftIO $ documentNewFromFile (show $ fileURI f') Nothing) ?-> parseE PDFParseE
  ps <- liftIO $ documentGetNPages doc
  liftIO $ mapM (pageGetText <=< documentGetPage doc) [0..pred ps]

pdf2text' :: (Monad m, MonadCatchIO m, Functor m, MonadCatchIO (EitherT GenError m)) => String -> GenErrorT m [String]
pdf2text' d = do
  t <- liftIO $ getTemporaryDirectory
  f' <- liftIO $ openBinaryTempFile t "convert.pdf" >>= (\p -> hClose (snd p) >> return (fst p))
  liftIO $ do
    h <- openBinaryFile f' ReadWriteMode
    hPutStr h d 
    hClose h
  pdf2textF' f' <* liftIO (removeFile f')
  
pdf2text :: (Proxy p) => () -> Pipe (EitherP GenError p) String [String] IO ()
pdf2text () = runIdentityPE $ request () >>= toEitherP . pdf2text' >>= respond

fetchHttp' :: String -> GenErrorT IO String
fetchHttp' url = withSource (urlURI url) $ do
  uri <- hoistMaybe (parseURI url) ?-> networkE URLInvalidE
  resE <- tryEither (_IOException.description) (simpleHTTP (mkRequest GET uri)) ?-= networkE . ConnOpenE
  res <- hoistEither resE ?-= networkE . ConnectionE
  case res of
    (Response (2,_,_) _ _ c) -> return c
    (Response (3,_,_) _ h _) -> do
      next <- (hoistMaybe $ headMay $ locs h) ?-> (networkE $ responseE $ MissingHeader HdrLocation)
      fetchHttp' next ?-= ioErr
    (Response code _ _ _) -> hoistEither $ Left $ networkE $ RequestE code
    where locs = map hdrValue . filter ((==HdrLocation) . hdrName)
          
fetchHttp :: Proxy p => String -> () -> Producer (EitherP GenError p) String IO ()
fetchHttp s () = runIdentityPE $ toEitherP (fetchHttp' s) >>= respond

