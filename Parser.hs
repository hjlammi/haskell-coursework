module Parser where
import Text.ParserCombinators.ReadP
import Person
import Object
import Data

mainParser :: String -> Data
mainParser str = do
  Data [fst $ head $ readP_to_S movementParser str]

qParser :: String -> Data
qParser question = do
  fst $ head $ readP_to_S questionParser question

questionParser :: ReadP Data
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
      return (Data [Person name (Just location) Nothing])
    else
      pfail


movementParser :: ReadP Person
movementParser = do
  name <- nameParser
  verb <- verbMoveParser
  location <- locationParser
  eof
  return (Person name (Just location) Nothing)

gettingObjectParser :: ReadP Person
gettingObjectParser = do
  name <- nameParser
  verb <- verbGetObjectParser
  object <- objectParser
  return (Person name Nothing (Just object))

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

objectParser :: ReadP Object
objectParser = do
  string "the"
  satisfy (== ' ')
  object <- readSubStr
  eof
  return Object { objectName = object }

readSubStr :: ReadP String
readSubStr = munch (\char -> char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z')

isInList :: String -> [String] -> Bool
isInList word words
  | elem word words = True
  | otherwise       = False
