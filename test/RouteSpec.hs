module ParserSpec where

import Test.Hspec
import Route
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec $ do
  describe "addRoom" $ do
    it "adds an empty route from a room to itself" $
      let routes = Map.empty
          expected = Map.fromList [(("bedroom", "bedroom"), [])] in
      addRoom "bedroom" routes `shouldBe` expected

  describe "addRoom" $ do
    it "doesn't add new room as the room already exists" $
      let routes = Map.fromList [(("bedroom", "bedroom"), [])] in
      addRoom "bedroom" routes `shouldBe` routes

  describe "extendRoute" $ do
    it "doesn't extend an old route because the new route is not related to the old one" $
      let oldRoute = (("bedroom", "bedroom"), [])
          expected = [(("bedroom", "bedroom"), [])] in
      extendRoute "hallway" "toilet" North oldRoute `shouldBe` expected

  describe "extendRoute" $ do
    it "adds a new route from a room to it's neighboring room" $
      let oldRoute = (("bedroom", "bedroom"), [])
          expected = [(("bedroom", "bedroom"), []), (("bedroom", "toilet"), [North])] in
      extendRoute "bedroom" "toilet" North oldRoute `shouldBe` expected

  describe "extendRoute" $ do
    it "adds a new route that is related to the old route" $
      let oldRoute = (("bedroom", "toilet"), [North])
          expected = [(("bedroom", "toilet"), [North]), (("bedroom", "kitchen"), [North, East])] in
      extendRoute "toilet" "kitchen" East oldRoute `shouldBe` expected

  describe "extendRoute" $ do
    it "adds a new route that is related to the old route" $
      let oldRoute = (("toilet", "bedroom"), [South])
          expected = [(("toilet", "bedroom"), [South]), (("kitchen", "bedroom"), [West, South])] in
      extendRoute "toilet" "kitchen" East oldRoute `shouldBe` expected

  describe "extendRoutes" $ do
    it "adds a route to the routes map" $
      let routes = Map.fromList [(("bedroom", "bedroom"), [])]
          expected = Map.fromList [(("bedroom", "bedroom"), []), (("bedroom", "toilet"), [North])] in
      extendRoutes "bedroom" "toilet" North routes `shouldBe` expected

  describe "extendRoutes" $ do
    it "adds a route to the routes map" $
      let routes = Map.fromList [(("bedroom", "bedroom"), []), (("bedroom", "toilet"), [North])]
          expected = Map.fromList [(("bedroom", "bedroom"), []), (("bedroom", "toilet"), [North]), (("bedroom", "kitchen"), [North, East])] in
      extendRoutes "toilet" "kitchen" East routes `shouldBe` expected

  describe "oppositeDirection" $ do
    it "gets the opposite direction for a direction" $
      oppositeDirection East `shouldBe` West
