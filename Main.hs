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
        answerQuestion parsedData

answerQuestion :: Data -> IO ()
answerQuestion parsedData = do
  question <- getLine
  when (question /= "quit") $ do
    let answer = answerOne parsedData question in do
      print answer
      answerQuestion parsedData

parseLine :: String -> Person.Fact
parseLine line = parse $ line

parseLines :: [String] -> [Person.Fact]
parseLines lines =
  map parseLine lines

updateData :: Data -> Person.Fact -> Data
updateData dataElem (Person.PersonMovesFact f) =
  let name = Person.personName f
      newLocation = Person.personLocation f
      maybePerson = Map.lookup name $ persons dataElem
  in case maybePerson of
    Just person ->
      let updatedPerson = Person.updateLocation person newLocation
      in (Data (insertPerson updatedPerson $ persons dataElem) (objects dataElem))
    Nothing ->
      let newPerson = Person.Person name (Just newLocation) []
      in (Data (insertPerson newPerson $ persons dataElem) (objects dataElem))

updateData dataElem (Person.PersonTakesObjectFact f) =
  let name = Person.personTakesObjectName f
      object = Person.personTakesObjectObject f
      maybePerson = Map.lookup name (persons dataElem)
  in case maybePerson of
      Just person ->
        let updatedPerson = Person.updateObjects person object
        in (Data (insertPerson updatedPerson $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just name) Nothing) $ objects dataElem))
      Nothing ->
        let newName = Person.personTakesObjectName f
            newObject = Person.personTakesObjectObject f
            newPerson = (Person.Person newName Nothing [object])
        in (Data (insertPerson newPerson $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newName) Nothing) $ objects dataElem))

updateData dataElem (Person.PersonDiscardsObjectFact f) =
  let name = Person.personDiscardsObjectName f
      object = Person.personDiscardsObjectObject f
      maybePerson = Map.lookup name (persons dataElem)
  in case maybePerson of
    Just person ->
      let updatedPerson = Person.discardObject person f
      in (Data (insertPerson updatedPerson $ persons dataElem) (Map.delete object $ objects dataElem))
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
              newPerson = Person.updateObjects newOwner object
          in (Data (insertPerson newPerson $ persons updatedDataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects updatedDataElem))
        Nothing ->
          let updatedData = updateData dataElem (Person.PersonDiscardsObjectFact $ Person.PersonDiscardsObject oldOwnerName object)
              newPerson = (Person.Person newOwnerName Nothing [object])
          in (Data (insertPerson newPerson $ persons updatedData) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects updatedData))
    Nothing ->
      let maybeNewOwner = Map.lookup newOwnerName (persons dataElem)
      in case maybeNewOwner of
        Just newOwner ->
          let updatedOwner = Person.updateObjects newOwner object
              updatedData = (Data (insertPerson updatedOwner $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects dataElem))
              oldOwner = (Person.Person oldOwnerName Nothing [])
          in (Data (insertPerson oldOwner $ persons updatedData) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects updatedData))
        Nothing ->
          let oldOwner = (Person.Person oldOwnerName Nothing [])
              newOwner = (Person.Person newOwnerName Nothing [object])
              oldOwnerAddedData = (Data (insertPerson oldOwner $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects dataElem))
          in (Data (insertPerson newOwner $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects dataElem))

insertPerson :: Person.Person -> Map.Map String Person.Person -> Map.Map String Person.Person
insertPerson person persons =
  Map.insert (Person.name person) person $ persons

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
