module Core.Error (connectionE, requestE, GenError, GenErrorT) where

import Network.Stream
import Network.HTTP.Base
import Control.Monad.Error as E

data GenError = ConError ConnError | RequestError ResponseCode String | ErrorMsg String | ErrorUnkn
instance Error GenError where
  noMsg = ErrorUnkn
  strMsg = ErrorMsg
  
type GenErrorT m a = E.ErrorT GenError m a
  
connectionE :: ConnError -> GenError
connectionE = ConError

requestE :: ResponseCode -> String -> GenError
requestE = RequestError

