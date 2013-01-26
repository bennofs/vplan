module Bindings.Manos.Schedule where
       
import Core.PDF
import Core.Network
import Core.Schedule
import Core.Error
import Control.Monad.Error

fetchText :: String -> GenErrorT IO [String]
fetchText = (>>= pdf2text) . fetch

