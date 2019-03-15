module Parser where
import Text.ParserCombinators.ReadP

name :: ReadP String
name = do
  name <- many1 $ satisfy (\char -> char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z')
  satisfy (== ' ')
  return name
