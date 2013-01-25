module Core.Network where

import Core.Error
import Network.HTTP
import Control.Monad.Error
import System.IO

fetch :: String -> ErrorT GenError IO String
fetch f = do
  x <- liftIO $ simpleHTTP $ getRequest f
  case x of
    (Left e) -> throwError $ connectionE e
    (Right (Response (2,_,_) _ h b)) -> do
      lift $ print h
      return b
    (Right (Response (3,_,_) _ h _)) -> msum . map (fetch . hdrValue) $ filter ((==HdrLocation) . hdrName) h
    (Right (Response c r _ _)) ->  throwError $ requestE c r
  
