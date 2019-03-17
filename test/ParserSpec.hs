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

  describe "verbParser" $ do
    it "discards the verb followed by preposition to" $
      readP_to_S verbParser "moved to the office" `shouldBe` [((), "the office")]
