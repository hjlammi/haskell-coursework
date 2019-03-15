module PersonSpec where

import Test.Hspec
import Person

main :: IO ()
main = hspec $ do
  describe "getName" $ do
    it "returns the name of the person" $
      getName (Person {name = "John", location = Just "garden"}) `shouldBe` Just "John"

  describe "setLocation" $ do
    it "returns the person when their location is set" $
      setLocation (Person {name = "John", location = Nothing}) "garden" `shouldBe` (Person {name = "John", location = Just "garden"})

  describe "getLocation" $ do
    it "returns the location of the person" $
      getLocation (Person {name = "John", location = Just "garden"}) `shouldBe` Just "garden"
