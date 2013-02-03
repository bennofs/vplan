module Main where

import Core.Resource
import Data.List
import Control.Proxy
import Control.Proxy.Trans.Either

main :: IO ()
main = do
  x <- runProxy $ runEitherK $ fetchHttp "http://manos-dresden.de/schule/stundenplaene/stundenplan5-10.pdf" >-> pdf2text >-> mapD (intercalate "\n") >-> putStrLnD
  case x of
    (Left e) -> print e
    _ -> return ()
    