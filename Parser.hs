module Parser where
import Text.ParserCombinators.ReadP
import Person

mainParser :: ReadP Person
mainParser = do
  name <- nameParser
  verb <- verbParser
  location <-locationParser
  return Person { name = name, location = (Just location) }

nameParser :: ReadP String
nameParser = do
  name <- readSubStr
  satisfy (== ' ')
  if name == "The" then
    pfail
  else
    return name

verbParser :: ReadP ()
verbParser = do
  verb <- readSubStr
  if isInList verb ["moved", "went", "journeyed", "travelled"]
    then do
      satisfy (== ' ')
      string "to"
      satisfy (== ' ')
      return ()
    else
      pfail

locationParser :: ReadP String
locationParser = do
  string "the"
  satisfy (== ' ')
  location <- readSubStr
  eof
  return location

readSubStr :: ReadP String
readSubStr = many1 $ satisfy (\char -> char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z')

isInList :: String -> [String] -> Bool
isInList word words
  | elem word words = True
  | otherwise       = False
