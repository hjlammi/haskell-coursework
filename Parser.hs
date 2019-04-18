module Parser where
import Text.ParserCombinators.ReadP
import Control.Applicative
import Person
import Object
import Data
import qualified Data.Map.Strict as Map

parse :: String -> Fact
parse str = fst $ head $ readP_to_S (movementParser <|> takingObjectParser <|> discardingObjectParser <|> handingObjectParser <|> movingAwayParser <|> eitherLocationParser) str

parseQuestion :: String -> Question
parseQuestion question = do
  fst $ head $ readP_to_S questionParser question

questionParser :: ReadP Question
questionParser = do
  q <- readSubStr
  case q of
    "Is" -> do
      satisfy (== ' ')
      name <- nameParser
      satisfy (== ' ')
      string "in"
      satisfy (== ' ')
      location <- locationParser
      string " ?"
      return (PersonQuestion $ PQ name location)
    "Where" -> do
      satisfy (== ' ')
      string "is the "
      object <- readSubStr
      string " ?"
      return (ObjectQuestion $ OQ object)
    "How" -> do
      string " many objects is "
      name <- nameParser
      string " carrying ?"
      return (NumOfObjectsQuestion $ NumQ name)
    _ ->
      pfail


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
  string "the"
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
