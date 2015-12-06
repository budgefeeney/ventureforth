{-# LANGUAGE OverloadedStrings #-}
module VForth.GameTextSpec (
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

import VForth.GameText

newtype TestableTitle = TestableTitle Title deriving Show

right :: Show l => Either l r -> r
right (Right r) = r
right (Left  l) = error ("Unexpectedly encountered left-value " <> show l)

instance Arbitrary TestableTitle where
  arbitrary =
    let
      anyText   = arbitrary
      validText = suchThat anyText (isRight . title)
    in
      (TestableTitle . right . title) <$> validText

newtype TestableDescription = TestableDescription Description deriving Show

instance Arbitrary TestableDescription where
  arbitrary =
    let
      anyText   = arbitrary
      validText = suchThat anyText (isRight . description)
    in
      (TestableDescription . right . description) <$> validText

spec :: Spec
spec = return ()
