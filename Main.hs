-- These are just notes for myself about how the program could be developed further:
-- TODO: locationHistory: ["kitchen", Nothing, "park"] -> Where was X before park? don't know
-- TODO: locationHistory in case of either or: ["kitchen", ["park", "backyard"]] -> Where was X after kitchen? "Either park or backyard"

module Main where

import System.IO
import System.Environment
import Control.Monad
import Parser
import Data
import Fact
import Question
import qualified Person as Person
import Object
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Function ((&))

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args in do
    contents <- readFile fileName
    let listOfLines = lines contents
        listOfFacts = parseLines listOfLines
        parsedData = foldl updateData (Data Map.empty Map.empty) listOfFacts in do
        -- debug lines for seeing what kind of data has been read and stored:
        -- print listOfLines
        -- print parsedData
        answerQuestion parsedData

-- Loop for answering questions that are typed in the command-line
-- The program can be stopped by typing 'quit'
answerQuestion :: Data -> IO ()
answerQuestion parsedData = do
  question <- getLine
  when (question /= "quit") $ do
    let parsedQuestion = parseQuestion question
        answer = answerOne parsedData parsedQuestion in do
      print answer
      answerQuestion parsedData

parseLine :: String -> Maybe Fact
parseLine line = parseFact $ line

-- If a line from the input file cannot be parsed into a fact,
-- a Nothing is returned from the parser. parseLines function removes those
-- Nothing instances from the list of facts.
parseLines :: [String] -> [Fact]
parseLines lines =
  let listWithNothings = map parseLine lines
  in catMaybes listWithNothings

-- Updates an existing person's location or adds a new person with the location.
updateData :: Data -> Fact -> Data
updateData dataElem (Fact.PersonMovesFact f) =
  let name = Fact.personName f
      newLocation = Fact.personLocation f
      maybePerson = Map.lookup name $ persons dataElem
  in case maybePerson of
    Just person ->
      let updatedPerson = Person.updateLocations [newLocation] person
      in (Data (insertPerson updatedPerson $ persons dataElem) (objects dataElem))
    Nothing ->
      let newPerson = Person.Person name [newLocation] [[newLocation]] []
      in (Data (insertPerson newPerson $ persons dataElem) (objects dataElem))

-- Removes a location from a person moving away from the location.
updateData dataElem (Fact.PersonMovesAwayFact f) =
  let name = Fact.personMovesAwayName f
      location = Fact.personMovesAwayLocation f
      maybePerson = Map.lookup name $ persons dataElem
      in case maybePerson of
        Just person ->
          let updatedPerson = Person.removeLocation person location
          in Data (insertPerson updatedPerson $ persons dataElem) (objects dataElem)
        Nothing ->
          -- The new person is no longer in the location but the location is stored in their history
          let newPerson = (Person.Person name [] [[location]] [])
          in Data (insertPerson newPerson $ persons dataElem) (objects dataElem)

-- Updates person to have two possible locations
updateData dataElem (Fact.PersonEitherLocationFact f) =
  let name = Fact.personEitherLocationName f
      locations = Fact.personEitherLocationLocations f
      maybePerson = Map.lookup name $ persons dataElem
      in case maybePerson of
        Just person ->
          let updatedPerson = Person.updateLocations locations person
          in Data (insertPerson updatedPerson $ persons dataElem) (objects dataElem)
        Nothing ->
          let newPerson = (Person.Person name locations [locations] [])
          in Data (insertPerson newPerson $ persons dataElem) (objects dataElem)

-- Adds an object to a person
updateData dataElem (Fact.PersonTakesObjectFact f) =
  let name = Fact.personTakesObjectName f
      object = Fact.personTakesObjectObject f
      maybePerson = Map.lookup name (persons dataElem)
  in case maybePerson of
      Just person ->
        let updatedPerson = Person.updateObjects person object
        in (Data (insertPerson updatedPerson $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just name) Nothing) $ objects dataElem))
      Nothing ->
        let newName = Fact.personTakesObjectName f
            newObject = Fact.personTakesObjectObject f
            newPerson = (Person.Person newName [] [] [object])
        in (Data (insertPerson newPerson $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newName) Nothing) $ objects dataElem))

-- Removes the object from a person
updateData dataElem (Fact.PersonDiscardsObjectFact f) =
  let name = Fact.personDiscardsObjectName f
      object = Fact.personDiscardsObjectObject f
      maybePerson = Map.lookup name (persons dataElem)
  in case maybePerson of
    Just person ->
      let updatedPerson = Person.discardObject person f
      in (Data (insertPerson updatedPerson $ persons dataElem) (Map.delete object $ objects dataElem))
    Nothing ->
      -- A new person is added without the discarded object
      let newPerson = (Person.Person name [] [] [])
      in (Data (insertPerson newPerson $ persons dataElem) (objects dataElem))

-- Person hands an object to another person. If the persons do not exist in the data before, they have to be added there.
updateData dataElem (Fact.PersonHandsObjectFact f) =
  let oldOwnerName = Fact.personHandsObjectName f
      newOwnerName = Fact.personGetsObjectName f
      object = Fact.personGetsObjectObject f
      maybeOldOwner = Map.lookup oldOwnerName (persons dataElem)
  in case maybeOldOwner of
    Just oldOwner ->
      let maybeNewOwner = Map.lookup newOwnerName (persons dataElem)
      in case maybeNewOwner of
        Just newOwner ->
          let updatedDataElem = updateData dataElem (Fact.PersonDiscardsObjectFact $ Fact.PersonDiscardsObject oldOwnerName object)
              newPerson = Person.updateObjects newOwner object
          in (Data (insertPerson newPerson $ persons updatedDataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects updatedDataElem))
        Nothing ->
          let updatedData = updateData dataElem (Fact.PersonDiscardsObjectFact $ Fact.PersonDiscardsObject oldOwnerName object)
              location = Person.currentLocation oldOwner
              newPerson = (Person.Person newOwnerName location [location] [object])
          in (Data (insertPerson newPerson $ persons updatedData) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects updatedData))
    Nothing ->
      let maybeNewOwner = Map.lookup newOwnerName (persons dataElem)
      in case maybeNewOwner of
        Just newOwner ->
          let updatedOwner = Person.updateObjects newOwner object
              updatedData = (Data (insertPerson updatedOwner $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects dataElem))
              oldOwner = (Person.Person oldOwnerName [] [] [])
          in (Data (insertPerson oldOwner $ persons updatedData) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects updatedData))
        Nothing ->
          let oldOwner = (Person.Person oldOwnerName [] [] [])
              newOwner = (Person.Person newOwnerName [] [] [object])
              oldOwnerAddedData = (Data (insertPerson oldOwner $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects dataElem))
          in (Data (insertPerson newOwner $ persons dataElem) (Map.insert object (Object $ ObjectLocation (Just newOwnerName) Nothing) $ objects dataElem))

-- Not implemented
updateData dataElem (Fact.RouteFact f) =
  dataElem

-- A helper function for inserting a person in the persons map
insertPerson :: Person.Person -> Map.Map String Person.Person -> Map.Map String Person.Person
insertPerson person persons =
  Map.insert (Person.name person) person $ persons

-- Answers a question regarding a person's location
answerOne :: Data -> Maybe Question -> String
answerOne parsedData (Just (PersonLocationQuestion q)) =
  let maybePerson = Map.lookup (personLocationName q) (persons parsedData)
      questionLocation = personLocationLocation q
  in case maybePerson of
    Just person
      | length (Person.currentLocation person) > 1 && elem questionLocation (Person.currentLocation person) -> "maybe"
      | elem questionLocation (Person.currentLocation person) -> "yes"
      | (null (Person.currentLocation person)) && (null (Person.locationHistory person)) -> "maybe"
      | (null (Person.currentLocation person)) && (not (questionLocation == head (last (Person.locationHistory person)))) -> "maybe"
      | questionLocation == head (last (Person.locationHistory person)) -> "no"
      | not $ elem questionLocation (Person.currentLocation person) -> "no"
      | otherwise -> "maybe"
    Nothing -> "maybe"

-- Answers a question regarding the location of an object
answerOne parsedData (Just (ObjectLocationQuestion q)) =
  let maybeObject = Map.lookup (objectName q) (objects parsedData)
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
                  head $ Person.currentLocation person
            Nothing -> "don't know"
    Nothing -> "don't know"

-- Answers how many objects a person has on them
answerOne parsedData (Just (NumOfObjectsQuestion q)) =
  let maybePerson = Map.lookup (ownerName q) (persons parsedData)
  in case maybePerson of
    Just person ->
      show (Person.countObjects person) :: String
    Nothing -> "don't know"

-- Answers a question about the previous location of a person
answerOne parsedData (Just (PersonLocationBeforeQuestion q)) =
  let maybePerson = Map.lookup (personLocationBeforeName q) (persons parsedData)
  in case maybePerson of
    Just person ->
      let locations = Person.locationHistory person
          location = personLocationBeforeLocation q
          maybeLocation = findLocationBefore locations location
      in case maybeLocation of
        Just l -> l
        Nothing -> "don't know"
    Nothing -> "don't know"

-- Answers a question about the next location of a person
answerOne parsedData (Just (PersonLocationAfterQuestion q)) =
  let maybePerson = Map.lookup (personLocationAfterName q) (persons parsedData)
  in case maybePerson of
    Just person ->
      let locations = Person.locationHistory person
          location = personLocationAfterLocation q
          maybeLocation = findLocationAfter locations location
      in case maybeLocation of
        Just l -> l
        Nothing -> "don't know"
    Nothing -> "don't know"

-- If a question cannot be parsed, we cannot answer it
answerOne parsedData Nothing = "don't know"

answerOne parsedData _ = "don't know"

findLocationBefore :: [[String]] -> String -> Maybe String
findLocationBefore [] _ = Nothing
findLocationBefore [x] _ = Nothing
findLocationBefore (x:xs) location
  | location == head (head xs) = Just (head x)
  | otherwise = findLocationBefore xs location

findLocationAfter :: [[String]] -> String -> Maybe String
findLocationAfter [] _ = Nothing
findLocationAfter [x] location = Nothing
findLocationAfter (x:xs) location
  | location == head x = Just (head $ head xs)
  | otherwise = findLocationAfter xs location
