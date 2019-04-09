module Parser where
import Text.ParserCombinators.ReadP
import Person
import Object
import Data
import qualified Data.Map.Strict as Map

parse :: String -> Fact
parse str = do
  PersonMovesFact $ fst $ head $ readP_to_S movementParser str

parseQuestion :: String -> Question
parseQuestion question = do
  fst $ head $ readP_to_S questionParser question

questionParser :: ReadP Question
questionParser = do
  q <- readSubStr
  if q == "Is"
    then do
      satisfy (== ' ')
      name <- nameParser
      string "in"
      satisfy (== ' ')
      location <- locationParser
      string " ?"
      return (Question name location)
    else
      pfail


movementParser :: ReadP PersonMoves
movementParser = do
  name <- nameParser
  verb <- verbMoveParser
  location <- locationParser
  eof
  return (PersonMoves name location)

gettingObjectParser :: ReadP PersonTakesObject
gettingObjectParser = do
  name <- nameParser
  verb <- verbGetObjectParser
  object <- objectParser
  return (PersonTakesObject name object)

nameParser :: ReadP String
nameParser = do
  name <- readSubStr
  satisfy (== ' ')
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

verbGetObjectParser :: ReadP ()
verbGetObjectParser = do
  verb <- readSubStr
  if isInList verb ["took", "got"]
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
  eof
  return object

readSubStr :: ReadP String
readSubStr = munch (\char -> char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z')

isInList :: String -> [String] -> Bool
isInList word words
  | elem word words = True
  | otherwise       = False
