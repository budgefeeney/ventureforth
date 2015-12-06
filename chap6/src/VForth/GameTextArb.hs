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



newtype TestableTitle = TestableTitle Title deriving Show

right :: Show l => Either l r -> r
right (Right r) = r
right (Left  l) = error ("Unexpectedly encountered left-value " <> show l)

instance Arbitrary TestableTitle where
  arbitrary =
    let
      validChars  = elements (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ",.:;£$-!?&()' ")
      validString = choose (3, 30)
                    >>= \count -> replicateM count validChars
      validText   = Text.pack <$> validString
    in
      (TestableTitle . right . title) <$> validText

newtype TestableDescription = TestableDescription Description deriving Show

instance Arbitrary TestableDescription where
  arbitrary =
    let
      -- Quickcheck prefers to create small lists, but we need at least
      -- 30 characters for our description to be valid. If we relied on
      -- plain arbitrary we'd discard thousands of samples in the
      -- suchThat function, so we build them up into lists
      shortText = arbitrary :: Gen Text
      longText  = Text.concat <$> replicateM 30 shortText
      validText = suchThat longText (isRight . description)
    in
      (TestableDescription . right . description) <$> validText

spec :: Spec
spec = return ()
