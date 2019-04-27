-- TODO: locationHistory: ["kitchen", Nothing, "park"] -> Where was X before park? don't know
-- TODO: locationHistory in case of either or: ["kitchen", ["park", "backyard"]] -> Where was X after kitchen? "Either park or backyard"

module Main where

import System.IO
import System.Environment
import Control.Monad
import Parser
import Data
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
        print listOfLines
        print parsedData
        answerQuestion parsedData

answerQuestion :: Data -> IO ()
answerQuestion parsedData = do
  question <- getLine
  when (question /= "quit") $ do
    let parsedQuestion = parseQuestion question
        answer = answerOne parsedData parsedQuestion in do
      print answer
      answerQuestion parsedData

parseLine :: String -> Maybe Person.Fact
parseLine line = parseFact $ line

parseLines :: [String] -> [Person.Fact]
parseLines lines =
  let listWithNothings = map parseLine lines
  in catMaybes listWithNothings

updateData :: Data -> Person.Fact -> Data
updateData dataElem (Person.PersonMovesFact f) =
  let name = Person.personName f
      newLocation = Person.personLocation f
      maybePerson = Map.lookup name $ persons dataElem
  in case maybePerson of
    Just person ->
      let updatedPerson = Person.updateLocations [newLocation] person
      in (Data (insertPerson updatedPerson $ persons dataElem) (objects dataElem))
    Nothing ->
      let newPerson = Person.Person name [newLocation] [[newLocation]] []
      in (Data (insertPerson newPerson $ persons dataElem) (objects dataElem))

updateData dataElem (Person.PersonMovesAwayFact f) =
  let name = Person.personMovesAwayName f
      location = Person.personMovesAwayLocation f
      maybePerson = Map.lookup name $ persons dataElem
      in case maybePerson of
        Just person ->
          let updatedPerson = Person.removeLocation person location
          in Data (insertPerson updatedPerson $ persons dataElem) (objects dataElem)
        Nothing ->
          let newPerson = (Person.Person name [] [[location]] [])
          in Data (insertPerson newPerson $ persons dataElem) (objects dataElem)

updateData dataElem (Person.PersonEitherLocationFact f) =
  let name = Person.personEitherLocationName f
      locations = Person.personEitherLocationLocations f
      maybePerson = Map.lookup name $ persons dataElem
      in case maybePerson of
        Just person ->
          -- TODO: remove old location
          let updatedPerson = Person.updateLocations locations person
          in Data (insertPerson updatedPerson $ persons dataElem) (objects dataElem)
      -- TODO: Nothing

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
            newPerson = (Person.Person newName [] [] [object])
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
updateData dataElem (Person.RouteFact f) =
  dataElem

insertPerson :: Person.Person -> Map.Map String Person.Person -> Map.Map String Person.Person
insertPerson person persons =
  Map.insert (Person.name person) person $ persons

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
                  -- TODO: WHAT IF SEVERAL LOCATIONS
                  head $ Person.currentLocation person
            Nothing -> "don't know"
    Nothing -> "don't know"

answerOne parsedData (Just (NumOfObjectsQuestion q)) =
  let maybePerson = Map.lookup (ownerName q) (persons parsedData)
  in case maybePerson of
    Just person ->
      show (Person.countObjects person) :: String
    Nothing -> "don't know"

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
