module ParserSpec where

import Text.ParserCombinators.ReadP
import Test.Hspec
import Parser
import Person
import Object
import Data

main :: IO ()
main = hspec $ do
  describe "parseFactMaybe" $ do
    it "returns the Just result of parsing a fact" $
      parseFactMaybe movementParser "John moved to the office" `shouldBe` Just (PersonMovesFact $ PersonMoves "John" "office")

  describe "parseQuestionMaybe" $ do
    it "returns Nothing as parsing the question fails" $
      parseQuestionMaybe questionParser "Who is Mary with ?" `shouldBe` Nothing

  describe "nameParser" $ do
    it "returns the name of the person from the input string" $
      readP_to_S nameParser "John moved to the office" `shouldBe` [("John", " moved to the office")]

  describe "nameParser" $ do
    it "returns an empty string because no name is found" $
      readP_to_S nameParser "The bathroom is east of the bedroom" `shouldBe` []

  describe "isInList" $ do
    it "returns True because the word is included in the list of words" $
      isInList "moved" ["went", "moved"] `shouldBe` True

  describe "isInList" $ do
    it "returns False because the word is included in the list of words" $
      isInList "came" ["went", "moved"] `shouldBe` False

  describe "verbMoveParser" $ do
    it "discards the verb followed by preposition to" $
      readP_to_S verbMoveParser "moved to the office" `shouldBe` [((), "the office")]

  describe "verbMoveParser" $ do
    it "fails if given invalid verb" $
      readP_to_S verbMoveParser "pliiped to the office" `shouldBe` []

  describe "verbMoveParser" $ do
    it "should return empty tuple i.e. work with the word went" $
      readP_to_S verbMoveParser "went to the office" `shouldBe` [((), "the office")]

  describe "verbMoveParser" $ do
    it "should return empty tuple i.e. work with the word journeyed" $
      readP_to_S verbMoveParser "journeyed to the office" `shouldBe` [((), "the office")]

  describe "verbMoveParser" $ do
    it "should return empty tuple i.e. work with the word travelled" $
      readP_to_S verbMoveParser "travelled to the school" `shouldBe` [((), "the school")]

  describe " verbTakeObjectParser" $ do
    it "should return empty tuple i.e. work with the word took" $
      readP_to_S verbTakeObjectParser "took the football" `shouldBe` [((), "the football")]

  describe " verbTakeObjectParser" $ do
    it "should parseFact the work 'picked up'" $
      readP_to_S verbTakeObjectParser "picked up the football" `shouldBe` [((), "the football")]

  describe "locationParser" $ do
    it "should return school as the location" $
      readP_to_S locationParser "the school" `shouldBe` [("school", "")]

  describe "movementParser" $ do
    it "returns the name John and the current place office" $
      readP_to_S movementParser "John moved to the office" `shouldBe` [(PersonMovesFact $ PersonMoves "John" "office", "")]

  describe "movementParser" $ do
    it "returns the name Daniel and the current place bedroom" $
      readP_to_S movementParser "Daniel journeyed to the bedroom" `shouldBe` [(PersonMovesFact $ PersonMoves "Daniel" "bedroom", "")]

  describe "movementParser" $ do
    it "doesn't return anything" $
      readP_to_S movementParser "The garden is south of the bedroom" `shouldBe` []

  describe "objectParser" $ do
    it "returns the object someone has" $
      readP_to_S objectParser "the football" `shouldBe` [("football", "")]

  describe "takingObjectParser" $ do
    it "returns the name Mary and football as the object" $
      readP_to_S takingObjectParser "Mary took the football" `shouldBe` [(PersonTakesObjectFact $ PersonTakesObject "Mary" "football", "")]

  describe "takingObjectParser" $ do
    it "returns the name Mary and apple as the object" $
      readP_to_S takingObjectParser "Mary got the apple" `shouldBe` [(PersonTakesObjectFact $ PersonTakesObject "Mary" "apple", "")]

  describe "parseFact" $ do
    it "returns the name Mary and bedroom as the current place" $
      parseFact "Mary journeyed to the bedroom" `shouldBe` Just (PersonMovesFact $ PersonMoves "Mary" "bedroom")

  describe "parseFact" $ do
    it "returns the name Mary and football as the object" $
      parseFact "Mary took the football" `shouldBe` Just (PersonTakesObjectFact $ PersonTakesObject "Mary" "football")

  describe "parseFact" $ do
    it "returns direction from a room to another" $
      parseFact "The bathroom is east of the bedroom" `shouldBe` Just (RouteFact $ Route "bedroom" "bathroom" "east")

  describe "questionParser" $ do
    it "returns parsed question in a list" $
      readP_to_S questionParser "Is Mary in the kitchen ?" `shouldBe` [(PersonLocationQuestion $ PersonLocationQ "Mary" "kitchen", "")]

  describe "questionParser" $ do
    it "returns parsed question with football as the objectName" $
      readP_to_S questionParser "Where is the football ?" `shouldBe` [(ObjectLocationQuestion $ ObjectLocationQ "football", "")]

  describe "parseQuestion" $ do
    it "returns parsed question with Daniel as the personName and bedroom as the place" $
      parseQuestion "Is Daniel in the bedroom ?" `shouldBe` Just (PersonLocationQuestion $ PersonLocationQ "Daniel" "bedroom")

  describe "parseQuestion" $ do
    it "returns parsed question with football as the personName" $
      parseQuestion "Where is the football ?" `shouldBe` Just (ObjectLocationQuestion $ ObjectLocationQ "football")

  describe "discardingObjectParser" $ do
    it "returns the name Mary and apple as the object" $
      readP_to_S discardingObjectParser "Mary discarded the apple" `shouldBe` [(PersonDiscardsObjectFact $ PersonDiscardsObject "Mary" "apple", "")]

  describe "handingObjectParser" $ do
    it "returns the name Daniel and apple as the object" $
      readP_to_S handingObjectParser "Mary handed the apple to Daniel" `shouldBe` [(PersonHandsObjectFact $ PersonHandsObject "Mary" "Daniel" "apple", "")]

  describe "questionParser" $ do
    it "returns parsed question with Mary as the personName" $
      readP_to_S questionParser "How many objects is Mary carrying ?" `shouldBe` [(NumOfObjectsQuestion $ NumOfObjectsQ "Mary", "")]

  describe "questionParser" $ do
    it "returns parsed question that wants to know where Mary was before cinema" $
      readP_to_S questionParser "Where was Mary before the school ?" `shouldBe` [(PersonLocationBeforeQuestion $ PersonLocationBeforeQ "Mary" "school", "")]

  describe "questionParser" $ do
    it "returns parsed question that wants to know where Mary was after school" $
      readP_to_S questionParser "Where was Mary after the school ?" `shouldBe` [(PersonLocationAfterQuestion $ PersonLocationAfterQ "Mary" "school", "")]

  describe "isInLocationQuestionParser" $ do
    it "parses question asking if Mary is in the kitchen" $
      readP_to_S isInLocationQuestionParser "Is Mary in the kitchen ?" `shouldBe` [(PersonLocationQuestion $ PersonLocationQ "Mary" "kitchen", "")]

  describe "whereIsObjectQuestionParser" $ do
    it "parses question asking where the football is" $
      readP_to_S whereIsObjectQuestionParser "Where is the football ?" `shouldBe` [(ObjectLocationQuestion $ ObjectLocationQ "football", "")]

  describe "whereWasPersonBeforeQuestionParser" $ do
    it "parses question asking where Sandra was" $
      readP_to_S whereWasPersonBeforeQuestionParser "Where was Sandra before the school ?" `shouldBe` [(PersonLocationBeforeQuestion $ PersonLocationBeforeQ "Sandra" "school", "")]

  describe "howManyObjectsQuestionParser" $ do
    it "parses question asking how many objects Mary has" $
      readP_to_S howManyObjectsQuestionParser "How many objects is Mary carrying ?" `shouldBe` [(NumOfObjectsQuestion $ NumOfObjectsQ "Mary", "")]

  describe "movementParser" $ do
    it "returns the name Bill and the current place office" $
      readP_to_S movementParser "Bill is in the office" `shouldBe` [(PersonMovesFact $ PersonMoves "Bill" "office", "")]

  describe "movingAwayParser" $ do
    it "returns the name Fred and no current place" $
      readP_to_S movingAwayParser "Fred is no longer in the park" `shouldBe` [(PersonMovesAwayFact $ PersonMovesAway "Fred" "park", "")]

  describe "eitherLocationParser" $ do
    it "returns the name Fred and two possible locations" $
      readP_to_S eitherLocationParser "Fred is either in the school or the kitchen" `shouldBe` [(PersonEitherLocationFact $ PersonEitherLocation "Fred" ["school", "kitchen"], "")]

  describe "routeQuestionParser" $ do
    it "parses question asking how to get from a location to another" $
      readP_to_S routeQuestionParser "How do you go from the garden to the office ?" `shouldBe` [(RouteQuestion $ RouteQ "garden" "office", "")]
