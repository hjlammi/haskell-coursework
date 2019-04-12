module ParserSpec where

import Text.ParserCombinators.ReadP
import Test.Hspec
import Parser
import Person
import Object
import Data

main :: IO ()
main = hspec $ do
  describe "nameParser" $ do
    it "returns the name of the person from the input string" $
      readP_to_S nameParser "John moved to the office" `shouldBe` [("John", "moved to the office")]

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

  describe " verbGetObjectParser" $ do
    it "should return empty tuple i.e. work with the word took" $
      readP_to_S verbGetObjectParser "took the football" `shouldBe` [((), "the football")]

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

  describe "parse" $ do
    it "returns the name Mary and bedroom as the current place" $
      parse "Mary journeyed to the bedroom" `shouldBe` PersonMovesFact (PersonMoves "Mary" "bedroom")

  describe "questionParser" $ do
    it "returns parsed question in a list" $
      readP_to_S questionParser "Is Mary in the kitchen ?" `shouldBe` [(Question "Mary" (Just "kitchen"), "")]

  describe "questionParser" $ do
    it "returns parsed question with football as the subject" $
      readP_to_S questionParser "Where is the football ?" `shouldBe` [(Question "football" Nothing, "")]

  describe "parseQuestion" $ do
    it "returns parsed question with Daniel as the subject and bedroom as the place" $
      parseQuestion "Is Daniel in the bedroom ?" `shouldBe` (Question "Daniel" (Just "bedroom"))

  describe "parseQuestion" $ do
    it "returns parsed question with football as the subject" $
      parseQuestion "Where is the football ?" `shouldBe` (Question "football" Nothing)
