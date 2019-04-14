module PersonSpec where

import Test.Hspec
import Person

main :: IO ()
main = hspec $ do
  describe "updateLocation" $ do
    it "updates person's location to a new one" $
      let fact = (PersonMoves "John" "bathroom")
          updatedPerson = Person "John" (Just "bathroom") [] in
      updateLocation fact `shouldBe` updatedPerson

  describe "discard" $ do
    it "returns empty list when trying to discard from an empty list" $
      discard "apple" [] `shouldBe` []

  describe "discard" $ do
    it "discards the only object from the list" $
      discard "apple" ["apple"] `shouldBe` []

  describe "discard" $ do
    it "discards the first object from the list" $
      discard "apple" ["apple", "flower"] `shouldBe` ["flower"]

  describe "discard" $ do
    it "tries to discard an object that isn't in the list" $
      discard "apple" ["orange"] `shouldBe` ["orange"]

  describe "discard" $ do
    it "discards last object from the list" $
      discard "apple" ["orange", "apple"] `shouldBe` ["orange"]

  describe "discard" $ do
    it "discards a middle object from the list" $
      discard "apple" ["orange", "apple", "pear"] `shouldBe` ["orange", "pear"]

  describe "discard" $ do
    it "discards a second last object from the list of 4 objects" $
      discard "apple" ["avokado", "orange", "apple", "pear"] `shouldBe` ["avokado", "orange", "pear"]

  describe "discard" $ do
    it "discards the second object from the list of 4 objects" $
      discard "apple" ["orange", "apple", "pear", "avokado"] `shouldBe` ["orange", "pear", "avokado"]

  describe "discardObject" $ do
    it "discards person's only object" $
      let person = Person "John" (Just "garden") ["apple"]
          fact = (PersonDiscardsObject "John" "apple")
          updatedPerson = (Person "John" (Just "garden") []) in
      discardObject person fact `shouldBe` updatedPerson

  describe "discardObject" $ do
    it "discards person's first object" $
      let person = Person "John" (Just "garden") ["apple", "flower"]
          fact = (PersonDiscardsObject "John" "apple")
          updatedPerson = (Person "John" (Just "garden") ["flower"]) in
      discardObject person fact `shouldBe` updatedPerson

  describe "discardObject" $ do
    it "discards person's last object" $
      let person = Person "John" (Just "garden") ["apple", "flower"]
          fact = (PersonDiscardsObject "John" "flower")
          updatedPerson = (Person "John" (Just "garden") ["apple"]) in
      discardObject person fact `shouldBe` updatedPerson

  describe "discardObject" $ do
    it "doesn't discard anything because the object isn't in the list" $
      let person = Person "John" (Just "garden") ["apple", "flower", "football"]
          fact = (PersonDiscardsObject "John" "shoe")
          updatedPerson = (Person "John" (Just "garden") ["apple", "flower", "football"]) in
      discardObject person fact `shouldBe` updatedPerson

  describe "countObjects" $ do
    it "returns 0 because the person doesn't have any objects" $
      let person = Person "John" (Just "garden") [] in
      countObjects person `shouldBe` 0

  describe "countObjects" $ do
    it "returns 1 because the person has one object" $
      let person = Person "John" (Just "garden") ["apple"] in
      countObjects person `shouldBe` 1

  describe "countObjects" $ do
    it "returns 3 because the person has three objects" $
      let person = Person "John" (Just "garden") ["apple", "football", "flower"] in
      countObjects person `shouldBe` 3
