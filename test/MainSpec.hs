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
  describe "answerOne" $ do
    it "answers yes when asked if Mary is in the kitchen" $
      Main.answerOne Data {person = Person {name = "Mary", location = Just "kitchen", object = Nothing}} "Is Mary in the kitchen ?" `shouldBe` "yes"

  describe "answerOne" $ do
    it "answers no when asked if Mary is in the garden" $
      Main.answerOne Data {person = Person {name = "Mary", location = Just "kitchen", object = Nothing}} "Is Mary in the garden ?" `shouldBe` "no"

  describe "readLinesToData" $ do
    it "returns an empty list if input is an empty list" $
      Main.readLinesToData [] `shouldBe` []

  describe "readLinesToData" $ do
    it "returns a list of one Data element formed from input of one line" $
      Main.readLinesToData ["John moved to the office"] `shouldBe` [Data {person = Person {name = "John", location = Just "office", object = Nothing}}]

  describe "readLinesToData" $ do
    it "returns a list of two Data element formed from input of two lines" $
      Main.readLinesToData ["John moved to the office", "Daniel journeyed to the bedroom"] `shouldBe` [Data {person = Person {name = "John", location = Just "office", object = Nothing}}, Data {person = Person {name = "Daniel", location = Just "bedroom", object = Nothing}}]
