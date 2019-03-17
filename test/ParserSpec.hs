module ParserSpec where

import Text.ParserCombinators.ReadP
import Test.Hspec
import Parser

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

  describe "verbParser" $ do
    it "discards the verb followed by preposition to" $
      readP_to_S verbParser "moved to the office" `shouldBe` [((), "the office")]

  describe "verbParser" $ do
    it "fails if given invalid verb" $
      readP_to_S verbParser "pliiped to the office" `shouldBe` []

  describe "verbParser" $ do
    it "should return empty tuple i.e. work with the word went" $
      readP_to_S verbParser "went to the office" `shouldBe` [((), "the office")]

  describe "verbParser" $ do
    it "should return empty tuple i.e. work with the word journeyed" $
      readP_to_S verbParser "journeyed to the office" `shouldBe` [((), "the office")]

  describe "verbParser" $ do
    it "should return empty tuple i.e. work with the word travelled" $
      readP_to_S verbParser "travelled to the school" `shouldBe` [((), "the school")]

  describe "locationParser" $ do
    it "should return school as the location" $
      readP_to_S locationParser "the school" `shouldBe` [("school", "")]
