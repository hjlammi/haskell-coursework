module ParserSpec where

import Text.ParserCombinators.ReadP
import Test.Hspec
import qualified Main
import Parser
import Person
import Object
import Data

main :: IO ()
main = hspec $ do

-- answerOne
  describe "answerOne" $ do
    it "answers yes when asked if Mary is in the kitchen" $
      Main.answerOne (Data  $ Person "Mary" (Just "kitchen") Nothing) "Is Mary in the kitchen ?" `shouldBe` "yes"

  describe "answerOne" $ do
    it "answers no when asked if Mary is in the garden" $
      Main.answerOne (Data  $ Person "Mary" (Just "kitchen") Nothing) "Is Mary in the garden ?" `shouldBe` "no"

-- replace
  describe "replace" $ do
    it "returns a list with a data item with the location of the person John as office" $
      Main.replace [Data $ Person "John" (Just "garden") Nothing] (Data $ Person "John" (Just "office") Nothing) `shouldBe` [(Data $ Person "John" (Just "office") Nothing)]

  describe "replace" $ do
    it "changes the last data element in the list" $
      Main.replace [Data $ Person "Mary" (Just "garden") Nothing, Data $ Person "John" (Just "garden") Nothing, Data $ Person "Lisa" (Just "bathroom") Nothing] (Data $ Person "John" (Just "office") Nothing) `shouldBe` [Data $ Person "Mary" (Just "garden") Nothing, Data $ Person "John" (Just "office") Nothing, Data $ Person "Lisa" (Just "bathroom") Nothing]

  describe "replace" $ do
    it "changes the last data element in the list" $
      Main.replace [Data $ Person "Mary" (Just "garden") Nothing, Data $ Person "Lisa" (Just "bathroom") Nothing, Data $ Person "John" (Just "garden") Nothing] (Data $ Person "John" (Just "office") Nothing) `shouldBe` [Data $ Person "Mary" (Just "garden") Nothing, Data $ Person "Lisa" (Just "bathroom") Nothing, Data $ Person "John" (Just "office") Nothing]

  describe "readLinesToData" $ do
    it "returns an empty list if input is an empty list" $
      Main.readLinesToData [] [] `shouldBe` []

  describe "readLinesToData" $ do
    it "returns a list of one Data element formed from input of one line" $
      Main.readLinesToData ["John moved to the office"] [] `shouldBe` [Data $ Person "John" (Just "office") Nothing]

  describe "readLinesToData" $ do
    it "returns a list of two Data element formed from input of two lines" $
      Main.readLinesToData ["John moved to the office", "Daniel journeyed to the bedroom"] [] `shouldBe` [Data $ Person "John" (Just "office") Nothing, Data $ Person "Daniel" (Just "bedroom") Nothing]

  -- describe "readLinesToData" $ do
  --   it "returns a list of one Data element formed from input of one line" $
  --     Main.readLinesToData ["John moved to the office", "John went to the bedroom"] [] `shouldBe` [Data {person = Person {name = "John", location = Just "bedroom", object = Nothing}}]
