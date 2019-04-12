module Main where

import System.IO
import System.Environment
import Control.Monad
import Parser
import Data
import Person
import Object
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args in do
    contents <- readFile fileName
    let listOfLines = lines contents
        listOfFacts = parseLines listOfLines
        parsedData = foldl updateData (Data Map.empty Map.empty) listOfFacts in do
        readQuestion parsedData

readQuestion :: Data -> IO ()
readQuestion parsedData = do
  question <- getLine
  when (question /= "quit") $ do
    let answer = answerOne parsedData question in do
      print answer
      readQuestion parsedData

parseLine :: String -> Fact
parseLine line = parse $ line

parseLines :: [String] -> [Fact]
parseLines lines =
  map parseLine lines

updateData :: Data -> Fact -> Data
updateData dataElem (PersonMovesFact f) =
  let name = personName f
      location = personLocation f
      updatedPerson = (Person name (Just location) Nothing)
  in (Data (Map.insert name updatedPerson $ persons dataElem) Map.empty)

updateData dataElem (PersonTakesObjectFact f) =
  let name = personTakesObjectName f
      object = personTakesObjectObject f
      updatedPerson = (Person name Nothing (Just object))
  in (Data (Map.insert name updatedPerson $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just name) Nothing) $ objects dataElem ))

answerOne :: Data -> String -> String
answerOne parsedData question =
  let parsedQuestion = parseQuestion question
  in case parsedQuestion of
    PersonQuestion pq ->
      let maybePerson = Map.lookup (subject pq) (persons parsedData)
      in case maybePerson of
        Just person ->
          if (location person) == (place pq)
          then "yes"
          else "no"
        Nothing -> "maybe"
    ObjectQuestion oq ->
      let maybeObject = Map.lookup (objectName oq) (objects parsedData)
      in case maybeObject of
        Just object ->
          let maybeInLocation = inLocation $ objectLocation object
          in case maybeInLocation of
            Just location -> location
            Nothing -> "don't know"
        Nothing -> "don't know"
