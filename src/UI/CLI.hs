{-# LANGUAGE DeriveDataTypeable #-}
import System.Environment
import System.Exit
import Control.Monad.Reader
import System.Console.CmdArgs
import System.Console.ANSI
import Data.List (intercalate)
import Data.Char (isNumber, isLetter, isLower)
import Bindings.Manos
import Core.Checker

data VPlanCheck = VPlanCheck { classes :: [String] } 
                  deriving (Show, Data, Typeable)

opts = VPlanCheck { 
  classes = def &= help "Specify class to include in the result. To specify multiple classes, just use this options multiple times" &= typ "CLASS_ID"
  }
       
formatChanges :: [TimetableChange] -> String
formatChanges = intercalate "\n" . map formatChange

pad :: Int -> String -> String
pad x s 
 | x > length s = s ++ (x - length s) `replicate` ' '
 | otherwise = s

formatChange :: TimetableChange -> String
formatChange x = concat (zipWith pad [10,10,10,10,10] x) ++ last x

getClasses :: VPlanCheck -> [String]
getClasses = concat . map genCls . classes
  where genCls x
          | length x == 0 = []
          | length x <= 2 && all isNumber x = concat $ genCls `map` (map (x++) ["a","b","c"]) 
          | length x == 2 && isNumber (head x) && isLower (last x) = ['0' : x]
          | otherwise = [x]
                        
filterImportantChanges :: [

main :: IO ()
main = do 
 args  <- cmdArgs opts
 allChanges <- fetchTable
 let changes = (filterImportantChanges allChanges) $ getClasses args
 if null changes 
   then setSGR [SetColor Foreground Vivid Green] >> putStrLn "Keine Änderungen" >> exitSuccess
   else return ()
 setSGR [SetColor Foreground Dull Yellow]
 putStrLn $ formatChanges [["Klasse", "Stunde", "Fach", "Lehrer", "Raum", "Info"]]
 setSGR [SetColor Foreground Vivid Yellow]
 putStrLn $ formatChanges changes