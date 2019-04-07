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
        question <- getLine
        let answer = answerOne parsedData question in do
          print answer

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

answerOne :: Data -> String -> String
answerOne parsedData question =
  let parsedQuestion = parseQuestion question
      maybePerson = Map.lookup (who parsedQuestion) (persons parsedData)
  in case maybePerson of
    Just person ->
      if (location person) == Just (place parsedQuestion)
      then "yes"
      else "no"
    Nothing -> "maybe"
