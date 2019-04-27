module Parser where
import Text.ParserCombinators.ReadP
import Control.Applicative
import Person
import Object
import Data
import Fact
import Question
import qualified Data.Map.Strict as Map

parseFactMaybe :: ReadP a -> String -> Maybe a
parseFactMaybe parser input =
  case readP_to_S parser input of
    ((result, _): _) -> Just result
    [] -> Nothing

parseQuestionMaybe :: ReadP a -> String -> Maybe a
parseQuestionMaybe qParser input =
  case readP_to_S qParser input of
    ((result, _): _) -> Just result
    [] -> Nothing

parseFact :: String -> Maybe Fact
parseFact str = parseFactMaybe (movementParser <|> takingObjectParser <|> discardingObjectParser <|> handingObjectParser <|> movingAwayParser <|> eitherLocationParser <|> routeParser) str

parseQuestion :: String -> Maybe Question
parseQuestion question = do
  parseQuestionMaybe questionParser question

questionParser :: ReadP Question
questionParser = (isInLocationQuestionParser <|> whereIsObjectQuestionParser <|> whereWasPersonBeforeQuestionParser <|> whereWasPersonAfterQuestionParser <|> howManyObjectsQuestionParser <|> routeQuestionParser)

isInLocationQuestionParser :: ReadP Question
isInLocationQuestionParser = do
  string "Is "
  name <- nameParser
  string " in "
  location <- locationParser
  string " ?"
  return (PersonLocationQuestion $ PersonLocationQ name location)

whereIsObjectQuestionParser :: ReadP Question
whereIsObjectQuestionParser = do
  string "Where is the "
  object <- readSubStr
  string " ?"
  return (ObjectLocationQuestion $ ObjectLocationQ object)

whereWasPersonBeforeQuestionParser :: ReadP Question
whereWasPersonBeforeQuestionParser = do
  string "Where was "
  name <- nameParser
  string " before "
  location <- locationParser
  string " ?"
  return (PersonLocationBeforeQuestion $ PersonLocationBeforeQ name location)

whereWasPersonAfterQuestionParser :: ReadP Question
whereWasPersonAfterQuestionParser = do
  string "Where was "
  name <- nameParser
  string " after "
  location <- locationParser
  string " ?"
  return (PersonLocationAfterQuestion $ PersonLocationAfterQ name location)

howManyObjectsQuestionParser :: ReadP Question
howManyObjectsQuestionParser = do
  string "How many objects is "
  name <- nameParser
  string " carrying ?"
  return (NumOfObjectsQuestion $ NumOfObjectsQ name)

routeQuestionParser :: ReadP Question
routeQuestionParser = do
  string "How do you go from "
  from <- locationParser
  string " to "
  to <- locationParser
  string " ?"
  return (RouteQuestion $ RouteQ from to)

movementParser :: ReadP Fact
movementParser = do
  name <- nameParser
  satisfy (== ' ')
  verb <- verbMoveParser <|> verbIsParser
  location <- locationParser
  eof
  return (PersonMovesFact $ PersonMoves name location)

movingAwayParser :: ReadP Fact
movingAwayParser = do
  name <- nameParser
  string " is no longer in "
  location <- locationParser
  eof
  return (PersonMovesAwayFact $ PersonMovesAway name location)

takingObjectParser :: ReadP Fact
takingObjectParser = do
  name <- nameParser
  satisfy (== ' ')
  verb <- verbTakeObjectParser
  object <- objectParser
  eof
  return (PersonTakesObjectFact $ PersonTakesObject name object)

discardingObjectParser :: ReadP Fact
discardingObjectParser = do
  name <- nameParser
  satisfy (== ' ')
  verb <- verbDiscardObjectParser
  object <- objectParser
  eof
  return (PersonDiscardsObjectFact $ PersonDiscardsObject name object)

handingObjectParser :: ReadP Fact
handingObjectParser = do
  oldOwner <- nameParser
  satisfy (== ' ')
  verb <- verbTakeObjectParser
  object <- objectParser
  string " to "
  newOwner <- nameParser
  eof
  return (PersonHandsObjectFact $ PersonHandsObject oldOwner newOwner object)

eitherLocationParser :: ReadP Fact
eitherLocationParser = do
  name <- nameParser
  string " is either in "
  location1 <-locationParser
  string " or "
  location2 <- locationParser
  eof
  return (PersonEitherLocationFact $ PersonEitherLocation name [location1, location2])

routeParser :: ReadP Fact
routeParser = do
  to <- locationParser
  string " is "
  direction <- readSubStr
  string " of "
  from <- locationParser
  eof
  return (RouteFact $ Route from to direction)

nameParser :: ReadP String
nameParser = do
  name <- readSubStr
  if name == "The" then
    pfail
  else
    return name

verbMoveParser :: ReadP ()
verbMoveParser = do
  verb <- readSubStr
  if isInList verb ["moved", "went", "journeyed", "travelled"]
    then do
      satisfy (== ' ')
      string "to"
      satisfy (== ' ')
      return ()
    else
      pfail

verbIsParser :: ReadP ()
verbIsParser = do
  verb <- readSubStr
  if verb == "is"
    then do
      string " in "
      return ()
    else
      pfail

verbTakeObjectParser :: ReadP ()
verbTakeObjectParser = do
  string "took " <|> string "got " <|> string "picked up " <|> string "handed "
  return ()

verbDiscardObjectParser :: ReadP ()
verbDiscardObjectParser = do
  verb <- readSubStr
  if isInList verb ["discarded"]
    then do
      satisfy (== ' ')
      return ()
    else
      pfail

locationParser :: ReadP String
locationParser = do
  string "the" <|> string "The"
  satisfy (== ' ')
  location <- readSubStr
  return location

objectParser :: ReadP String
objectParser = do
  string "the"
  satisfy (== ' ')
  object <- readSubStr
  return object

readSubStr :: ReadP String
readSubStr = munch (\char -> char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z')

isInList :: String -> [String] -> Bool
isInList word words
  | elem word words = True
  | otherwise       = False
