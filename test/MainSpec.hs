module ParserSpec where

import Text.ParserCombinators.ReadP
import Test.Hspec
import qualified Main
import Parser
import Person
import Object
import Data
import Fact
import Question
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec $ do

-- parseLine
  describe "parseLine" $ do
    it "returns one PersonMovesFact formed from input of one line" $
      Main.parseLine "John moved to the office" `shouldBe` Just (PersonMovesFact $ PersonMoves "John" "office")

  describe "parseLine" $ do
    it "returns RouteFact formed from input of one line" $
      Main.parseLine "The bathroom is east of the bedroom" `shouldBe` Just (RouteFact $ Route "bedroom" "bathroom" "east")

-- parseLines
  describe "parseLines" $ do
    it "parses a list of one line into a list of one fact" $
      Main.parseLines ["John moved to the office"] `shouldBe` [(PersonMovesFact $ PersonMoves "John" "office")]

  describe "parseLines" $ do
    it "parses a list of two lines into a list of two facts" $
      Main.parseLines ["John moved to the office", "Mary went to the garden"] `shouldBe` [(PersonMovesFact $ PersonMoves "John" "office"), (PersonMovesFact $ PersonMoves "Mary" "garden")]

  describe "parseLines" $ do
    it "parses a list of two lines into a list of two facts" $
      Main.parseLines ["Mary journeyed to the bedroom", "Mary took the football"] `shouldBe` [(PersonMovesFact $ PersonMoves "Mary" "bedroom"), (PersonTakesObjectFact $ PersonTakesObject "Mary" "football")]

  describe "parseLines" $ do
    it "parses a list of three lines into a list of three facts" $
      Main.parseLines ["John moved to the office", "Mary went to the garden", "John moved to the kitchen"] `shouldBe` [(PersonMovesFact $ PersonMoves "John" "office"), (PersonMovesFact $ PersonMoves "Mary" "garden"), (PersonMovesFact $ PersonMoves "John" "kitchen")]

-- updateData
  describe "updateData" $ do
    it "returns an updated Data element in a Map with one person" $
      let d = Data
              (Map.fromList [("John", Person "John" ["kitchen"] [["kitchen"]] [])])
              Map.empty
          fact = (PersonMovesFact $ PersonMoves "John" "office")
          expected = Data
                     (Map.fromList [("John", Person "John" ["office"] [["kitchen"], ["office"]] [])])
                     Map.empty in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "returns an updated Data element in a Map with one person and objects" $
      let d = Data
              (Map.fromList [("John", Person "John" ["kitchen"] [["kitchen"]] ["apple", "football"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("football", Object $ ObjectLocation (Just "Mary") Nothing)])
          fact = (PersonMovesFact $ PersonMoves "John" "office")
          expected = Data
                     (Map.fromList [("John", Person "John" ["office"] [["kitchen"], ["office"]] ["apple", "football"])])
                     (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("football", Object $ ObjectLocation (Just "Mary") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "updates person in the middle of the persons Map" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] []), ("John", Person "John" ["kitchen"] [["kitchen"]] []), ("Lisa", Person "Lisa" ["garden"] [["garden"]] [])])
              Map.empty
          fact = (PersonMovesFact $ PersonMoves "John" "office")
          expected = Data
                     (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] []), ("John", Person "John" ["office"] [["kitchen"], ["office"]] []), ("Lisa", Person "Lisa" ["garden"] [["garden"]] [])])
                     Map.empty in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "updates last person in the Map" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] []), ("John", Person "John" ["kitchen"] [["kitchen"]] []), ("Lisa", Person "Lisa" ["garden"] [["garden"]] [])])
              Map.empty
          fact = (PersonMovesFact $ PersonMoves "Lisa" "office")
          expected = Data
                     (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] []), ("John", Person "John" ["kitchen"] [["kitchen"]] []), ("Lisa", Person "Lisa" ["office"] [["garden"], ["office"]] [])])
                     Map.empty in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "updates nonempty objects Map" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["flower"])])
              (Map.fromList [("flower", Object $ ObjectLocation (Just "Mary") Nothing)])
          fact = (PersonMovesFact $ PersonMoves "John" "office")
          expected = Data
                    (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["flower"]), ("John", Person "John" ["office"] [["office"]] [])])
                    (Map.fromList [("flower", Object $ ObjectLocation (Just "Mary") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "updates person's object and adds the object with its location in the objects Map" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] [])])
              Map.empty
          fact = (PersonTakesObjectFact $ PersonTakesObject "Mary" "apple")
          expected = Data
                    (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple"])])
                    (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "adds new person with an object in the objects list" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] [])])
              Map.empty
          fact = (PersonTakesObjectFact $ PersonTakesObject "John" "flower")
          expected = Data
                    (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] []), ("John", Person "John" [] [] ["flower"])])
                    (Map.fromList [("flower", Object $ ObjectLocation (Just "John") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "adds new object in the person's objects" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          fact = (PersonTakesObjectFact $ PersonTakesObject "Mary" "flower")
          expected = Data
                    (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple", "flower"])])
                    (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "adds the first and only object in a person's objects" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] [])])
              Map.empty
          fact = (PersonTakesObjectFact $ PersonTakesObject "Mary" "flower")
          expected = Data
                     (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["flower"])])
                     (Map.fromList [("flower", Object $ ObjectLocation (Just "Mary") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "discards the first and only object in a person's objects" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["flower"])])
              (Map.fromList [("flower", Object $ ObjectLocation (Just "Mary") Nothing)])
          fact = (PersonDiscardsObjectFact $ PersonDiscardsObject "Mary" "flower")
          expected = Data
                     (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] [])])
                     Map.empty in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "discards the last object in a person's objects" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple", "flower"])])
              (Map.fromList [("flower", Object $ ObjectLocation (Just "Mary") Nothing), ("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          fact = (PersonDiscardsObjectFact $ PersonDiscardsObject "Mary" "flower")
          expected = Data
                     (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple"])])
                     (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "discards the middle object in a person's objects" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple", "flower", "football"])])
              (Map.fromList [("football", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing), ("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          fact = (PersonDiscardsObjectFact $ PersonDiscardsObject "Mary" "flower")
          expected = Data
                    (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple", "football"])])
                    (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("football", Object $ ObjectLocation (Just "Mary") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "tries to discard object from a person that is not in the data" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple", "flower", "football"])])
              (Map.fromList [("football", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing), ("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          fact = (PersonDiscardsObjectFact $ PersonDiscardsObject "Sarah" "flower") in
      Main.updateData d fact `shouldBe` d

  describe "updateData" $ do
    it "adds an object to a new person Daniel handed by Mary" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          fact = (PersonHandsObjectFact $ PersonHandsObject "Mary" "Daniel" "apple")
          -- If Mary hands an object to Daniel we can deduct that they are in the same room -> Daniel's location
          expected = Data
                    (Map.fromList [("Daniel", Person "Daniel" ["kitchen"] [["kitchen"]] ["apple"]), ("Mary", Person "Mary" ["kitchen"] [["kitchen"]] [])])
                    (Map.fromList [("apple", Object $ ObjectLocation (Just "Daniel") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "adds an object to an existing person Daniel handed by Mary" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["flower", "apple"]), ("Daniel", Person "Daniel" ["kitchen"] [["kitchen"]] [])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing)])
          fact = (PersonHandsObjectFact $ PersonHandsObject "Mary" "Daniel" "apple")
          expected = Data
                    (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["flower"]), ("Daniel", Person "Daniel" ["kitchen"] [["kitchen"]] ["apple"])])
                    (Map.fromList [("apple", Object $ ObjectLocation (Just "Daniel") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "removes a location from a person as the person moves away from it" $
      let d = Data
              (Map.fromList [("Fred", Person "Fred" ["kitchen"] [["kitchen"]] ["flower"])])
              (Map.fromList [("flower", Object $ ObjectLocation (Just "Fred") Nothing)])
          fact = (PersonMovesAwayFact $ PersonMovesAway "Fred" "kitchen")
          expected = Data
                     (Map.fromList [("Fred", Person "Fred" [] [["kitchen"]] ["flower"])])
                     (Map.fromList [("flower", Object $ ObjectLocation (Just "Fred") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "tries to remove location from a person that doesn't exist" $
      let d = Data Map.empty Map.empty
          fact = (PersonMovesAwayFact $ PersonMovesAway "Fred" "park")
          expected = Data
                     (Map.fromList [("Fred", Person "Fred" [] [["park"]] [])])
                     Map.empty in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "adds person one location" $
      let d = Data
              (Map.fromList [("Fred", Person "Fred" [] [] ["flower"])])
              (Map.fromList [("flower", Object $ ObjectLocation (Just "Fred") Nothing)])
          fact = (PersonMovesFact $ PersonMoves "Fred" "park")
          expected = Data
                    (Map.fromList [("Fred", Person "Fred" ["park"] [["park"]] ["flower"])])
                    (Map.fromList [("flower", Object $ ObjectLocation (Just "Fred") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "adds person two possible locations" $
      let d = Data
              (Map.fromList [("Fred", Person "Fred" [] [] ["flower"])])
              (Map.fromList [("flower", Object $ ObjectLocation (Just "Fred") Nothing)])
          fact = (PersonEitherLocationFact $ PersonEitherLocation "Fred" ["park", "kitchen"])
          expected = Data
                     (Map.fromList [("Fred", Person "Fred" ["park", "kitchen"] [["park", "kitchen"]] ["flower"])])
                     (Map.fromList [("flower", Object $ ObjectLocation (Just "Fred") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "adds new person two possible locations" $
      let d = Data
              (Map.fromList [("Fred", Person "Fred" [] [] ["flower"])])
              (Map.fromList [("flower", Object $ ObjectLocation (Just "Fred") Nothing)])
          fact = (PersonEitherLocationFact $ PersonEitherLocation "Mary" ["park", "kitchen"])
          expected = Data
                     (Map.fromList [("Fred", Person "Fred" [] [] ["flower"]),
                                    ("Mary", Person "Mary" ["park", "kitchen"] [["park", "kitchen"]] [])])
                     (Map.fromList [("flower", Object $ ObjectLocation (Just "Fred") Nothing)]) in
      Main.updateData d fact `shouldBe` expected

  -- insertPerson
  describe "insertPerson" $ do
    it "adds a person in an empty persons Map" $
      let persons = Map.empty
          person = (Person.Person "Mary" [] [] [])
          expected = Map.fromList [("Mary", Person "Mary" [] [] [])]
      in Main.insertPerson person persons `shouldBe` expected

  -- answerOne
  describe "answerOne" $ do
    it "answers yes when asked if Mary is in the kitchen" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] [])])
              Map.empty
          q = Just (PersonLocationQuestion $ PersonLocationQ "Mary" "kitchen") in
        Main.answerOne d q `shouldBe` "yes"

  describe "answerOne" $ do
    it "answers no when asked if Mary is in the garden because Mary is in the kitchen" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] [])])
              Map.empty
          q = Just (PersonLocationQuestion $ PersonLocationQ "Mary" "garden") in
      Main.answerOne d q `shouldBe` "no"

  describe "answerOne" $ do
    it "answers maybe when asked if a person we have no data of is in a location" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" [] [] [])])
              Map.empty
          q = Just (PersonLocationQuestion $ PersonLocationQ "Sarah" "garden") in
      Main.answerOne d q `shouldBe` "maybe"

  describe "answerOne" $ do
    it "returns hallway as the location for football" $
      let d = Data
              Map.empty
              (Map.fromList [("football", Object $ ObjectLocation Nothing (Just "hallway"))])
          q = Just (ObjectLocationQuestion $ ObjectLocationQ "football") in
      Main.answerOne d q `shouldBe` "hallway"

  describe "answerOne" $ do
    it "returns kitchen as the location for football that John has" $
      let d = Data
              (Map.fromList [("John", Person "John" ["kitchen"] [["kitchen"]] [])])
              (Map.fromList [("football", Object $ ObjectLocation (Just "John") Nothing)])
          q = Just (ObjectLocationQuestion $ ObjectLocationQ "football") in
      Main.answerOne d q `shouldBe` "kitchen"

  describe "answerOne" $ do
    it "answers that the location is unknown" $
      let d = Data
              (Map.fromList [("John", Person "John" ["kitchen"] [["kitchen"]] [])])
              (Map.fromList [("apple", Object $ ObjectLocation Nothing Nothing)])
          q = Just (ObjectLocationQuestion $ ObjectLocationQ "apple") in
      Main.answerOne d q `shouldBe` "don't know"

  describe "answerOne" $ do
    it "answers that the location is unknown because such object is not in the Data" $
      let d = Data
              (Map.fromList [("John", Person "John" ["kitchen"] [["kitchen"]] [])])
              (Map.fromList [("apple", Object $ ObjectLocation Nothing Nothing)])
          q = Just (ObjectLocationQuestion $ ObjectLocationQ "football") in
      Main.answerOne d q `shouldBe` "don't know"

  describe "answerOne" $ do
    it "returns 0 as Mary doesn't have any objects" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] [])])
              Map.empty
          q = Just (NumOfObjectsQuestion $ NumOfObjectsQ "Mary") in
      Main.answerOne d q `shouldBe` "0"

  describe "answerOne" $ do
    it "returns 2 as Mary has an apple and a flower" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple", "flower"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (NumOfObjectsQuestion $ NumOfObjectsQ "Mary") in
      Main.answerOne d q `shouldBe` "2"

  describe "answerOne" $ do
    it "answers 'don't know' because the person is not in the list" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["kitchen"]] ["apple", "flower"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (NumOfObjectsQuestion $ NumOfObjectsQ "John") in
      Main.answerOne d q `shouldBe` "don't know"

  describe "answerOne" $ do
    it "answers 'maybe' because we don't know the person's current location" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" [] [["kitchen"]] ["apple", "flower"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (PersonLocationQuestion $ PersonLocationQ "Mary" "garden") in
      Main.answerOne d q `shouldBe` "maybe"

  describe "answerOne" $ do
    it "answers 'no' because the question location is the last location where the person just left" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" [] [["kitchen"]] ["apple", "flower"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (PersonLocationQuestion $ PersonLocationQ "Mary" "kitchen") in
      Main.answerOne d q `shouldBe` "no"

  describe "answerOne" $ do
    it "answers 'yes' because the person is in the question location" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen"] [["garden"], ["kitchen"]] ["apple", "flower"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (PersonLocationQuestion $ PersonLocationQ "Mary" "kitchen") in
      Main.answerOne d q `shouldBe` "yes"

  describe "answerOne" $ do
    it "answers 'maybe' because the person is either in the question location or in some other location" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["kitchen", "garden"] [["kitchen"], ["garden"]] ["apple", "flower"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (PersonLocationQuestion $ PersonLocationQ "Mary" "kitchen") in
      Main.answerOne d q `shouldBe` "maybe"

  describe "answerOne" $ do
    it "answers 'no' because the person is in some other location than what was asked" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["park", "garden"] [["kitchen"], ["park"], ["garden"]] ["apple", "flower"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing), ("flower", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (PersonLocationQuestion $ PersonLocationQ "Mary" "kitchen") in
      Main.answerOne d q `shouldBe` "no"

  describe "answerOne" $ do
    it "answers 'no' because the person is in some other location than what was asked" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["park"] [["park"]] ["apple"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Nothing in
      Main.answerOne d q `shouldBe` "don't know"

  describe "answerOne" $ do
    it "answers school which was the location before park" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["park"] [["school"], ["park"]] ["apple"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (PersonLocationBeforeQuestion $ PersonLocationBeforeQ "Mary" "park") in
      Main.answerOne d q `shouldBe` "school"

  describe "answerOne" $ do
    it "answers 'don't know' as Mary was nowhere before park" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["park"] [["park"]] ["apple"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (PersonLocationBeforeQuestion $ PersonLocationBeforeQ "Mary" "park") in
      Main.answerOne d q `shouldBe` "don't know"

  describe "answerOne" $ do
    it "answers school which was the location after home" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["school"] [["home"], ["school"]] ["apple"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (PersonLocationAfterQuestion $ PersonLocationAfterQ "Mary" "home") in
      Main.answerOne d q `shouldBe` "school"

  describe "answerOne" $ do
    it "answers 'don't know' because the location after last location is unknown" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" [] [["home"], ["school"]] ["apple"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (PersonLocationAfterQuestion $ PersonLocationAfterQ "Mary" "school") in
      Main.answerOne d q `shouldBe` "don't know"

  describe "answerOne" $ do
    it "answers 'don't know' because person was not in the data" $
      let d = Data
              (Map.fromList [("Mary", Person "Mary" ["school"] [["home"], ["school"], ["cinema"]] ["apple"])])
              (Map.fromList [("apple", Object $ ObjectLocation (Just "Mary") Nothing)])
          q = Just (PersonLocationAfterQuestion $ PersonLocationAfterQ "Sandra" "park") in
      Main.answerOne d q `shouldBe` "don't know"

-- findLocationBefore
  describe "findLocationBefore" $ do
    it "returns Nothing as the list doesn't have any locations" $
      let locations = []
          location = "office" in
      Main.findLocationBefore locations location `shouldBe` Nothing

  describe "findLocationBefore" $ do
    it "returns Nothing as the list doesn't have a location before the current one" $
      let locations = [["office"]]
          location = "office" in
      Main.findLocationBefore locations location `shouldBe` Nothing

  describe "findLocationBefore" $ do
    it "returns the first location of the list of two" $
      let locations = [["park"], ["office"]]
          location = "office" in
      Main.findLocationBefore locations location `shouldBe` Just "park"

  describe "findLocationBefore" $ do
    it "returns the middle location of the list" $
      let locations = [["park"], ["kitchen"], ["office"]]
          location = "office" in
      Main.findLocationBefore locations location `shouldBe` Just "kitchen"

  describe "findLocationBefore" $ do
    it "returns the first location of the list of three" $
      let locations = [["park"], ["kitchen"], ["office"]]
          location = "kitchen" in
      Main.findLocationBefore locations location `shouldBe` Just "park"

-- findLocationAfter
  describe "findLocationAfter" $ do
    it "returns Nothing as the list doesn't have any locations" $
      let locations = []
          location = "office" in
      Main.findLocationAfter locations location `shouldBe` Nothing

  describe "findLocationAfter" $ do
    it "returns Nothing as the list has only one location" $
      let locations = [["office"]]
          location = "office" in
      Main.findLocationAfter locations location `shouldBe` Nothing

  describe "findLocationAfter" $ do
    it "returns park as the location after office" $
      let locations = [["office"], ["park"]]
          location = "office" in
      Main.findLocationAfter locations location `shouldBe` Just "park"

  describe "findLocationAfter" $ do
    it "returns restaurant as the location after park" $
      let locations = [["office"], ["park"], ["restaurant"]]
          location = "park" in
      Main.findLocationAfter locations location `shouldBe` Just "restaurant"
