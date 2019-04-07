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

-- answerOne
  -- describe "answerOne" $ do
  --   it "answers yes when asked if Mary is in the kitchen" $
  --     Main.answerOne (Data [Person "Mary" (Just "kitchen") Nothing]) "Is Mary in the kitchen ?" `shouldBe` "yes"
  --
  -- describe "answerOne" $ do
  --   it "answers no when asked if Mary is in the garden" $
  --     Main.answerOne (Data [Person "Mary" (Just "kitchen") Nothing]) "Is Mary in the garden ?" `shouldBe` "no"

-- parseLine
  describe "parseLine" $ do
    it "returns one PersonMovesFact formed from input of one line" $
      Main.parseLine "John moved to the office" `shouldBe` (PersonMovesFact $ PersonMoves "John" "office")

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
