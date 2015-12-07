{-# LANGUAGE OverloadedStrings #-}
module VForth.GameTextArb (
    TestableTitle(..)
  , TestableDescription(..)
  , right
  , spec
) where

import Test.Hspec
import Test.QuickCheck.Gen
import Test.QuickCheck
import Data.Either (isRight, )
import Data.Monoid ((<>))
import Data.Text.Arbitrary ()
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad

import VForth.GameText


right :: Show l => Either l r -> r
right (Right r) = r
right (Left  l) = error ("Unexpectedly encountered left-value " <> show l)

arbitraryFromConstraints :: SafeTextConstraints -> (Text -> a) -> Gen a
arbitraryFromConstraints SafeTextConstraints{..} makeFn =
  let
    validCharGen   = elements validChars
    validStringGen = choose (minLength, maxLength)
                     >>= \count -> replicateM count validCharsGen
    validTextGen   = Text.pack <$> validStringGen
  in
    (makeFn . right . title) <$> validTextGen

newtype TestableTitle = TestableTitle Title deriving Show
instance Arbitrary TestableTitle where
  arbitrary = arbitraryFromConstraints titleConstraints TestableTitle

newtype TestableDescription = TestableDescription Description deriving Show
instance Arbitrary TestableDescription where
  arbitrary = arbitraryFromConstraints descriptionConstraints TestableDescription

spec :: Spec
spec = return ()
