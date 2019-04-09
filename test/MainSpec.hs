module ParserSpec where

import Text.ParserCombinators.ReadP
import Test.Hspec
import qualified Main
import Parser
import Person
import Object
import Data
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec $ do

-- parseLine
  describe "parseLine" $ do
    it "returns one PersonMovesFact formed from input of one line" $
      Main.parseLine "John moved to the office" `shouldBe` (PersonMovesFact $ PersonMoves "John" "office")

-- parseLines
  describe "parseLines" $ do
    it "parses a list of one line into a list of one fact" $
      Main.parseLines ["John moved to the office"] `shouldBe` [(PersonMovesFact $ PersonMoves "John" "office")]

  describe "parseLines" $ do
    it "parses a list of two lines into a list of two facts" $
      Main.parseLines ["John moved to the office", "Mary went to the garden"] `shouldBe` [(PersonMovesFact $ PersonMoves "John" "office"), (PersonMovesFact $ PersonMoves "Mary" "garden")]

  describe "parseLines" $ do
    it "parses a list of two lines into a list of two facts" $
      Main.parseLines ["Mary journeyed to the bedroom", "Mary took the football"] `shouldBe` [(PersonMovesFact $ PersonMoves "Mary" "bedroom"), (PersonMovesObjectFact $ PersonMovesObject "Mary" "football")]

  describe "parseLines" $ do
    it "parses a list of three lines into a list of three facts" $
      Main.parseLines ["John moved to the office", "Mary went to the garden", "John moved to the kitchen"] `shouldBe` [(PersonMovesFact $ PersonMoves "John" "office"), (PersonMovesFact $ PersonMoves "Mary" "garden"), (PersonMovesFact $ PersonMoves "John" "kitchen")]

-- updateData
  describe "updateData" $ do
    it "returns an updated Data element in a Map with one person" $
      let d = Data $ Map.fromList [("John", Person "John" (Just "kitchen") Nothing)]
          fact = (PersonMovesFact $ PersonMoves "John" "office")
          expected = Data $ Map.fromList [("John", Person "John" (Just "office") Nothing)] in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "updates person in the middle of the Map" $
      let d = Data $ Map.fromList [("Mary", Person "Mary" (Just "kitchen") Nothing), ("John", Person "John" (Just "kitchen") Nothing), ("Lisa", Person "Lisa" (Just "garden") Nothing)]
          fact = (PersonMovesFact $ PersonMoves "John" "office")
          expected = Data $ Map.fromList [("Mary", Person "Mary" (Just "kitchen") Nothing), ("John", Person "John" (Just "office") Nothing), ("Lisa", Person "Lisa" (Just "garden") Nothing)] in
      Main.updateData d fact `shouldBe` expected

  describe "updateData" $ do
    it "updates last person in the Map" $
      let d = Data $ Map.fromList [("Mary", Person "Mary" (Just "kitchen") Nothing), ("John", Person "John" (Just "kitchen") Nothing), ("Lisa", Person "Lisa" (Just "garden") Nothing)]
          fact = (PersonMovesFact $ PersonMoves "John" "office")
          expected = Data $ Map.fromList [("Mary", Person "Mary" (Just "kitchen") Nothing), ("John", Person "John" (Just "office") Nothing), ("Lisa", Person "Lisa" (Just "garden") Nothing)] in
      Main.updateData d fact `shouldBe` expected

  -- answerOne
    describe "answerOne" $ do
      it "answers yes when asked if Mary is in the kitchen" $
        let d = Data $ Map.fromList [("Mary", Person "Mary" (Just "kitchen") Nothing)] in
        Main.answerOne d "Is Mary in the kitchen ?" `shouldBe` "yes"

    describe "answerOne" $ do
      it "answers no when asked if Mary is in the garden" $
        let d = Data $ Map.fromList [("Mary", Person "Mary" (Just "kitchen") Nothing)] in
        Main.answerOne d "Is Mary in the garden ?" `shouldBe` "no"
