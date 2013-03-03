module Bindings.Manos.Covers (fetchTable, TimetableChange, getCourse, getTeacher, getLesson, getLessonNumber, getRoom, getInfo)  where

import Data.List (isPrefixOf)
import Control.Applicative ((<$>),(<*>))
import Data.List.Split (splitOn)
import Data.Char (isLower, isNumber)
import Data.Text (strip, pack, unpack)
import Text.XML.HXT.Core
import Text.XML.HXT.TagSoup (withTagSoup)
import Text.XML.HXT.HTTP (withHTTP)

-- |Configuration of the HXT Parser
hxtConfig = configSysVars 
  [ withHTTP [] 
  , withTrace (-1)
  , withTagSoup 
  , withValidate no
  , withWarnings no
  , withParseHTML yes
  , withInputEncoding "ISO-8859-1"
  ]

-- |An arrow that fetches the html document with the table
subPlanHtml :: IOSArrow b XmlTree
subPlanHtml = hxtConfig
              >>> traceMsg 0 "Start reading document"  
              >>> readDocument [] "http://manos-dresden.de/aktuelles/vplan.php"
              >>> traceMsg 0 "Done. Start processing document"

-- |The 'isClassIdentifier' function tests whether a given string is a valid identifier for a class
isClassIdentifier :: String -> Bool
isClassIdentifier str
 | length str == 3 = all ((&&) <$> isNumber <*> (`elem` "0156789")) (init str) && isLower (last str)
 | length str >= 4 = "JG" `isPrefixOf` str && all ((&&) <$> isNumber <*> (`elem` "012")) (take 2 $ drop 2 str)
 | otherwise = False

-- |An arrow that returns all rows in a table
tableRow :: ArrowXml a => a XmlTree XmlTree
tableRow = multi (hasName "tr")

-- |An arrow that returns the data of each row in a table
tableRowData :: ArrowXml a => a XmlTree XmlTree
tableRowData = getChildren >>> hasName "td"

-- |An arrow that extracts the substitution table from the html page
subTable :: ArrowXml a => a XmlTree XmlTree
subTable = tableRow </ (getChildren >>> hasText (any (isClassIdentifier . unpack . strip . pack) . splitOn ","))

-- |An arrow that filters all entries that describe changes in the timetable
subTableMainData :: (ArrowXml a) => a XmlTree XmlTree
subTableMainData = filterA mainItem
  where mainItem = listA tableRowData >>> isA ((==6) . length)

-- |An arrow that creates a table entry for each row
makeEntry :: (ArrowXml a) => a XmlTree [String]
makeEntry = listA $ tableRowData >>> deep getText

-- |An IO action that gets the table
fetchTable :: IO [[String]]
fetchTable = runX $ subPlanHtml >>> subTable >>> subTableMainData >>> makeEntry