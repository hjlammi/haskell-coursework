module ParserSpec where

import Text.ParserCombinators.ReadP
import Test.Hspec
import qualified Main
import Parser
import Person
import Object
import Data
import QuestionData

main :: IO ()
main = hspec $ do
  describe "answerOne" $ do
    it "answers yes when asked if Mary is in the kitchen" $
      Main.answerOne QuestionData {p = Person {name = "Mary", location = Just "kitchen", object = Nothing}} "Is Mary in the kitchen ?" `shouldBe` "yes"

  describe "answerOne" $ do
    it "answers no when asked if Mary is in the garden" $
      Main.answerOne QuestionData {p = Person {name = "Mary", location = Just "kitchen", object = Nothing}} "Is Mary in the garden ?" `shouldBe` "no"
