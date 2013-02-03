module Bindings.Manos.Schedule where
       
import Core.Resource
import Core.Schedule
import Core.Error
import Data.Time.Calendar
import Text.ParserCombinators.Poly
import Data.Traversable
import Control.Monad hiding (mapM)
import Prelude hiding (mapM)

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday deriving (Show)

token :: (Eq t) => t -> Parser t t
token = satisfy . (==)

grade :: Parser Char [Char]
grade = traverse token "10" <|> sequenceA [token '0', oneOf (map token ['5'..'9'])]

classId :: Parser Char [Char]
classId = (++) <$> grade <*> (fmap pure . oneOf . map token) "abc"

lineBreak :: Parser Char ()
lineBreak = void $ oneOf [token '\r' >> token '\n', token '\r', token '\n']

digit :: Parser Char Char
digit = oneOf $ map token ['0' .. '9']

parseDate :: Parser Char Day
parseDate = (dn <* token '.') <**> ((dn <* token '.') <**> (read <$> exactly 4 digit <&> fromGregorian))
  where dn = read <$> exactly 2 digit

pageHeader :: Parser Char Day
pageHeader = traverse token "Martin-Andersen-Nexö-Gymnasium Dresden, gültig ab " *> parseDate

dayOfWeek :: Parser Char DayOfWeek
dayOfWeek = oneOf . map (uncurry (*>)) . map ((_2 %~ pure) . (_1 %~ traverse token)) $ [
  ("Mo", Monday),
  ("Di", Tuesday),
  ("Mi", Wednesday),
  ("Do", Thursday),
  ("Fr", Friday)]
            

  