module PersonSpec where

import Test.Hspec
import Person

main :: IO ()
main = hspec $ do
  -- describe "updateLocation" $ do
  --   it "updates person's location to a new one" $
  --     let oldPerson = Person "John" ["kitchen"] [["kitchen"]] []
  --         updatedPerson = Person "John" ["bathroom"] [["kitchen"], ["bathroom"]] [] in
  --     updateLocation oldPerson ["bathroom"] `shouldBe` updatedPerson

  describe "updateLocations" $ do
    it "updates person's location to a new one" $
      let oldPerson = Person "John" ["kitchen"] [["kitchen"]] []
          updatedPerson = Person "John" ["bathroom"] [["kitchen"], ["bathroom"]] [] in
      updateLocations ["bathroom"] oldPerson `shouldBe` updatedPerson

  -- describe "updateLocation" $ do
  --   it "updates person's location to a new one while the objects remain the same" $
  --     let oldPerson = Person "John" ["kitchen"] [["kitchen"]] ["flower", "apple"]
  --         updatedPerson = Person "John" ["bathroom"] [["kitchen"], ["bathroom"]] ["flower", "apple"] in
  --     updateLocation oldPerson ["bathroom"] `shouldBe` updatedPerson

  describe "updateLocations" $ do
    it "updates person's location to a new one while the objects remain the same" $
      let oldPerson = Person "John" ["kitchen"] [["kitchen"]] ["flower", "apple"]
          updatedPerson = Person "John" ["bathroom"] [["kitchen"], ["bathroom"]] ["flower", "apple"] in
      updateLocations ["bathroom"] oldPerson `shouldBe` updatedPerson

  describe "removeLocation" $ do
    it "removes the only location from the person's location list" $
      let oldPerson = Person "John" ["kitchen"] [["kitchen"]] []
          updatedPerson = Person "John" [] [["kitchen"]] [] in
      removeLocation oldPerson "kitchen" `shouldBe` updatedPerson

  describe "removeLocation" $ do
    it "removes nothing because the location is not in the person's location list" $
      let oldPerson = Person "John" ["kitchen"] [["kitchen"]] []
          updatedPerson = Person "John" ["kitchen"] [["kitchen"]] [] in
      removeLocation oldPerson "bathroom" `shouldBe` updatedPerson

  describe "removeLocation" $ do
    it "removes nothing from an empty location list" $
      let oldPerson = Person "John" [] [] []
          updatedPerson = Person "John" [] [] [] in
      removeLocation oldPerson "bathroom" `shouldBe` updatedPerson

  -- describe "removeLocation" $ do
  --   it "removes the last one from a location list" $
  --     let oldPerson = Person "John" ["kitchen", "bathroom"] [["kitchen"], ["bathroom"]] []
  --         updatedPerson = Person "John" ["kitchen"] ["kitchen", ["bathroom"]] [] in
  --     removeLocation oldPerson "bathroom" `shouldBe` updatedPerson

  describe "updateCurrentLocation" $ do
    it "updates current location to empty list as the person leaves from the current location" $
      let oldPerson = Person "John" ["bathroom"] [["kitchen"], ["bathroom"]] []
          updatedPerson = Person "John" [] [["kitchen"], ["bathroom"]] [] in
      updateCurrentLocation [] oldPerson `shouldBe` updatedPerson

  describe "updateCurrentLocation" $ do
    it "updates current location as the person moves to another room" $
      let oldPerson = Person "John" ["bathroom"] [["kitchen"], ["bathroom"]] []
          updatedPerson = Person "John" ["hallway"] [["kitchen"], ["bathroom"]] [] in
      updateCurrentLocation ["hallway"] oldPerson `shouldBe` updatedPerson

  describe "updateCurrentLocation" $ do
    it "updates current location with two options" $
      let oldPerson = Person "John" ["bathroom"] [["kitchen"], ["bathroom"]] []
          updatedPerson = Person "John" ["hallway", "livingroom"] [["kitchen"], ["bathroom"]] [] in
      updateCurrentLocation ["hallway", "livingroom"] oldPerson `shouldBe` updatedPerson

  describe "updateLocationHistory" $ do
    it "adds new current location to location history" $
      let oldPerson = Person "John" ["hallway"] [["kitchen"], ["bathroom"]] []
          updatedPerson = Person "John" ["hallway"] [["kitchen"], ["bathroom"], ["hallway"]] [] in
      updateLocationHistory ["hallway"] oldPerson `shouldBe` updatedPerson

  describe "updateLocationHistory" $ do
    it "adds new pair of current locations to location history" $
      let oldPerson = Person "John" ["hallway", "livingroom"] [["kitchen"], ["bathroom"]] []
          updatedPerson = Person "John" ["hallway", "livingroom"] [["kitchen"], ["bathroom"], ["hallway", "livingroom"]] [] in
      updateLocationHistory ["hallway", "livingroom"] oldPerson `shouldBe` updatedPerson

  -- describe "addLocation" $ do
  --   it "adds one location" $
  --     let oldPerson = Person "John" ["bathroom"] [["bathroom"]] []
  --         updatedPerson = Person "John" ["bathroom", "kitchen"] [["bathroom", "kitchen"]] [] in
  --     addLocation oldPerson ["kitchen"] `shouldBe` updatedPerson

  -- describe "addLocation" $ do
  --   it "adds two locations" $
  --     let oldPerson = Person "John" ["bathroom"] ["bathroom"] []
  --         updatedPerson = Person "John" ["bathroom", "kitchen", "park"] ["bathroom", "kitchen", "park"] [] in
  --     addLocation oldPerson ["kitchen", "park"] `shouldBe` updatedPerson


  describe "updateObjects" $ do
    it "updates person's location" $
      let oldPerson = Person "John" ["kitchen"] [["kitchen"]] []
          expected = Person "John" ["kitchen"] [["kitchen"]] ["flower"] in
      updateObjects oldPerson "flower" `shouldBe` expected

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
      let person = Person "John" ["garden"] [["garden"]] ["apple"]
          fact = (PersonDiscardsObject "John" "apple")
          updatedPerson = (Person "John" ["garden"] [["garden"]] []) in
      discardObject person fact `shouldBe` updatedPerson

  describe "discardObject" $ do
    it "discards person's first object" $
      let person = Person "John" ["garden"] [["garden"]] ["apple", "flower"]
          fact = (PersonDiscardsObject "John" "apple")
          updatedPerson = (Person "John" ["garden"] [["garden"]] ["flower"]) in
      discardObject person fact `shouldBe` updatedPerson

  describe "discardObject" $ do
    it "discards person's last object" $
      let person = Person "John" ["garden"] [["garden"]] ["apple", "flower"]
          fact = (PersonDiscardsObject "John" "flower")
          updatedPerson = (Person "John" ["garden"] [["garden"]] ["apple"]) in
      discardObject person fact `shouldBe` updatedPerson

  describe "discardObject" $ do
    it "doesn't discard anything because the object isn't in the list" $
      let person = Person "John" ["garden"] [["garden"]] ["apple", "flower", "football"]
          fact = (PersonDiscardsObject "John" "shoe")
          updatedPerson = (Person "John" ["garden"] [["garden"]] ["apple", "flower", "football"]) in
      discardObject person fact `shouldBe` updatedPerson

  describe "countObjects" $ do
    it "returns 0 because the person doesn't have any objects" $
      let person = Person "John" ["garden"] [] [] in
      countObjects person `shouldBe` 0

  describe "countObjects" $ do
    it "returns 1 because the person has one object" $
      let person = Person "John" ["garden"] [] ["apple"] in
      countObjects person `shouldBe` 1

  describe "countObjects" $ do
    it "returns 3 because the person has three objects" $
      let person = Person "John" ["garden"] [] ["apple", "football", "flower"] in
      countObjects person `shouldBe` 3
