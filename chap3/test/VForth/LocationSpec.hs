module VForth.LocationSpec where

import Data.List (isInfixOf)
import Data.Char (isSpace)
import Test.Hspec
import Test.QuickCheck
import VForth

newtype TestableLocation = TestableLocation Location
instance Arbitrary TestableLocation where
  arbitrary = TestableLocation <$> (
    newLocation <$> arbitrary <*> arbitrary
    )
instance Show TestableLocation where
  show (TestableLocation l) = "Location { title=\"" ++ title l ++ "\", description=\"" ++ description l ++ "\" }"

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

    it "Title should be included in showable output" $ property $
      \(TestableLocation l) -> title l `isInfixOf` show l

    it "Description should be included in showable output" $ property $
      \(TestableLocation l) -> description l `isInfixOf` show l

    it "Showable output is never blank"  $ property $
      \(TestableLocation l) ->
        (not . null . title $ l) && (not . null . description $ l)
        ==> any (not . isSpace) (show l)
