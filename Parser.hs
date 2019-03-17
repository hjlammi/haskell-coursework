module Parser where
import Text.ParserCombinators.ReadP

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
  if not $ isInList verb ["moved", "went", "journeyed", "travelled"]
    then
      pfail
    else do
      satisfy (== ' ')
      string "to"
      satisfy (== ' ')
      return ()

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
