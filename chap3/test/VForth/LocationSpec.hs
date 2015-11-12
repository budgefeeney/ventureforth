module VForth.LocationSpec where

import Test.Hspec
import VForth

newLocation :: String -> String -> Location
newLocation titleText descText = Location {
    title = titleText
  , description = descText
  }

spec :: Spec
spec = do
  describe "Location Display" $ do
    it "Show puts title before dashes before a description" $ do
      show (newLocation "My Title" "The complete description.") `shouldBe` "My Title\n--------\nThe complete description."

    it "Title should be included in description" $ do
      show (newLocation "My Title" "The complete description.") `shouldContain` "My Title"

    it "Title should be included in description" $ do
      show (newLocation "My Title" "The complete description.") `shouldContain` "The complete description."
