module Core.PDF (pdf2text, pdf2textF) where

import Graphics.UI.Gtk.Poppler.Document (documentGetNPages, documentGetPage, documentNewFromFile)
import Graphics.UI.Gtk.Poppler.Page (pageGetText)
import Graphics.UI.Gtk.General.General (initGUI)
import Control.Monad ((<=<))
import Control.Applicative ((<$>))
import System.Glib.GError (GError)
import Control.Exception (handle, bracket)
import System.Directory (canonicalizePath, getTemporaryDirectory)
import System.IO (openBinaryTempFile, hClose, hPutStr, withBinaryFile, IOMode(..))
  
pdf2textF :: FilePath -> IO (Either GError [String])
pdf2textF f = handle (return . Left) $ do
  _ <- initGUI
  path <- canonicalizePath f
  (Just doc) <- documentNewFromFile ("file:" ++ path) Nothing
  ps <- documentGetNPages doc
  Right <$> mapM (pageGetText <=< documentGetPage doc) [0..pred ps]

pdf2text :: String -> IO (Either GError [String])
pdf2text d = do
  t <- getTemporaryDirectory
  f <- bracket
    (openBinaryTempFile t "convert.pdf")
    (hClose . snd) $ 
    return . fst
  withBinaryFile f WriteMode (\h -> hPutStr h d)
  pdf2textF f