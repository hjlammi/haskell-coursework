module ParserSpec where

import Text.ParserCombinators.ReadP
import Test.Hspec
import Parser

main :: IO ()
main = hspec $ do
  describe "name" $ do
    it "returns the name of the person from the input string" $
      readP_to_S name "John moved to the office" `shouldBe` [("John", "moved to the office")]

  describe "name" $ do
    it "returns an empty string because no name is found" $
      readP_to_S name "The bathroom is east of the bedroom" `shouldBe` [("", "bathroom is east of the bedroom")]
