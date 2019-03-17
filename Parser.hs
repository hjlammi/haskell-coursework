module Parser where
import Text.ParserCombinators.ReadP

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
  satisfy (== ' ')
  string "to"
  satisfy (== ' ')
  return ()

readSubStr = many1 $ satisfy (\char -> char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z')

isInList :: String -> [String] -> Bool
isInList word words
  | elem word words = True
  | otherwise       = False
