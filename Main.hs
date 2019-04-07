module Main where

import System.IO
import System.Environment
import Parser
import Data
import Person
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args in do
    contents <- readFile fileName
    let listOfLines = lines contents
        listOfFacts = parseLines listOfLines
        parsedData = foldl updateData (Data Map.empty) listOfFacts in do
          print parsedData

parseLine :: String -> Fact
parseLine line = parse $ line

parseLines :: [String] -> [Fact]
parseLines lines =
  map parseLine lines

updateData :: Data -> Fact -> Data
updateData dataElem (PersonMovesFact f)  =
  let name = personName f
      location = personLocation f
      updatedPerson = (Person name (Just location) Nothing)
  in (Data $ Map.insert name updatedPerson $ persons dataElem)

processFacts facts dataElem =
  let parsedLine = parseLine $ head facts in
    updateData dataElem parsedLine

-- answerOne :: Fact -> String -> String
-- answerOne parsedData question =
--   let parsedQuestion = qParser question in do
--     if parsedQuestion == parsedData
--       then "yes"
--     else
--       "no"
