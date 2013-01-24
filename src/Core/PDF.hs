module Core.PDF (pdf2text) where

import Graphics.UI.Gtk.Poppler.Document (documentGetNPages, documentGetPage, documentNewFromFile)
import Graphics.UI.Gtk.Poppler.Page (pageGetText)
import Graphics.UI.Gtk.General.General (initGUI)
import Control.Monad ((<=<))
import Control.Applicative ((<$>))
import System.Glib.GError (GError)
import Control.Exception (handle)
import System.Directory (canonicalizePath)
  
pdf2text :: FilePath -> IO (Either GError [String])
pdf2text f = handle (return . Left) $ do
  _ <- initGUI
  path <- canonicalizePath f
  (Just doc) <- documentNewFromFile ("file:" ++ path) Nothing
  ps <- documentGetNPages doc
  Right <$> mapM (pageGetText <=< documentGetPage doc) [0..pred ps]