module Parser where
import Text.ParserCombinators.ReadP

nameParser = do
  name <- many1 $ satisfy (\char -> char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z')
  satisfy (== ' ')
  if name == "The" then
    pfail
  else
    return name
