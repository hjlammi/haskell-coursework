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
