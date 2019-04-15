module Main where

import System.IO
import System.Environment
import Control.Monad
import Parser
import Data
import qualified Person as Person
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
        print listOfLines
        print parsedData
        readQuestion parsedData

readQuestion :: Data -> IO ()
readQuestion parsedData = do
  question <- getLine
  when (question /= "quit") $ do
    let answer = answerOne parsedData question in do
      print answer
      readQuestion parsedData

parseLine :: String -> Person.Fact
parseLine line = parse $ line

parseLines :: [String] -> [Person.Fact]
parseLines lines =
  map parseLine lines

updateData :: Data -> Person.Fact -> Data
updateData dataElem (Person.PersonMovesFact f) =
  let updatedPerson = Person.updateLocation f
  in (Data (Map.insert (Person.personName f) updatedPerson $ persons dataElem) (objects dataElem))

updateData dataElem (Person.PersonTakesObjectFact f) =
  let name = Person.personTakesObjectName f
      object = Person.personTakesObjectObject f
      maybePerson = Map.lookup name (persons dataElem)
  in case maybePerson of
      Just person ->
        let loc = Person.location person
            objs = Person.objects person
            updatedPerson = (Person.Person name loc (objs ++ [object]))
        in (Data (Map.insert name updatedPerson $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just name) Nothing) $ objects dataElem))
      Nothing ->
        let newName = Person.personTakesObjectName f
            newObject = Person.personTakesObjectObject f
            newPerson = (Person.Person newName Nothing [object])
        in (Data (Map.insert newName newPerson $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newName) Nothing) $ objects dataElem))

updateData dataElem (Person.PersonDiscardsObjectFact f) =
  let name = Person.personDiscardsObjectName f
      object = Person.personDiscardsObjectObject f
      maybePerson = Map.lookup name (persons dataElem)
  in case maybePerson of
    Just person ->
      let updatedPerson = Person.discardObject person f
      in (Data (Map.insert name updatedPerson $ persons dataElem) (Map.delete object $ objects dataElem))
    Nothing ->
      dataElem

updateData dataElem (Person.PersonHandsObjectFact f) =
  let oldOwnerName = Person.personHandsObjectName f
      newOwnerName = Person.personGetsObjectName f
      object = Person.personGetsObjectObject f
      maybeOldOwner = Map.lookup oldOwnerName (persons dataElem)
  in case maybeOldOwner of
    Just oldOwner ->
      let maybeNewOwner = Map.lookup newOwnerName (persons dataElem)
      in case maybeNewOwner of
        Just newOwner ->
          let updatedDataElem = updateData dataElem (Person.PersonDiscardsObjectFact $ Person.PersonDiscardsObject oldOwnerName object)
              newOwnerLoc = Person.location newOwner
              objs = Person.objects newOwner
              newPerson = (Person.Person newOwnerName newOwnerLoc (objs ++ [object]))
          in (Data (Map.insert newOwnerName newPerson $ persons updatedDataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects updatedDataElem))
        Nothing ->
          let updatedData = updateData dataElem (Person.PersonDiscardsObjectFact $ Person.PersonDiscardsObject oldOwnerName object)
              newPerson = (Person.Person newOwnerName Nothing [object])
          in (Data (Map.insert newOwnerName newPerson $ persons updatedData) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects updatedData))
    Nothing ->
      let maybeNewOwner = Map.lookup newOwnerName (persons dataElem)
      in case maybeNewOwner of
        Just newOwner ->
          let objs = Person.objects newOwner
              updatedOwner = (Person.Person newOwnerName Nothing (objs ++ [object]))
              updatedData = (Data (Map.insert newOwnerName updatedOwner $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects dataElem))
          in (Data (Map.insert oldOwnerName (Person.Person oldOwnerName Nothing []) $ persons updatedData) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects updatedData))
        Nothing ->
          let oldOwner = (Person.Person oldOwnerName Nothing [])
              newOwner = (Person.Person newOwnerName Nothing [object])
              oldOwnerAddedData = (Data (Map.insert oldOwnerName oldOwner $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects dataElem))
          in (Data (Map.insert newOwnerName newOwner $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects dataElem))

answerOne :: Data -> String -> String
answerOne parsedData question =
  let parsedQuestion = parseQuestion question
  in case parsedQuestion of
    PersonQuestion pq ->
      let maybePerson = Map.lookup (subject pq) (persons parsedData)
      in case maybePerson of
        Just person ->
          if (Person.location person) == (place pq)
          then "yes"
          else "no"
        Nothing -> "maybe"
    NumOfObjectsQuestion noq ->
      let maybePerson = Map.lookup (ownerName noq) (persons parsedData)
      in case maybePerson of
        Just person ->
          show (Person.countObjects person) :: String
        Nothing ->
          "don't know"
    ObjectQuestion oq ->
      let maybeObject = Map.lookup (objectName oq) (objects parsedData)
      in case maybeObject of
        Just object ->
          let maybeInLocation = inLocation $ objectLocation object
          in case maybeInLocation of
              Just location -> location
              Nothing ->
                let maybeOnPerson = onPerson $ objectLocation object
                in case maybeOnPerson of
                  Just personName ->
                    let maybePerson = Map.lookup personName (persons parsedData)
                    in case maybePerson of
                      Just person ->
                        let maybeLocation = Person.location person
                        in case maybeLocation of
                          Just location -> location
                          Nothing -> "don't know1"
                      Nothing -> "maybe"
        Nothing -> "don't know2"
