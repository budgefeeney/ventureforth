{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : VForth
Description : The game library for the Venture Forth game
Copyright   : (c) Bryan Feeney 2015

This is the library for the Venture Forth text-adventure game, containing
all game logic.
-}
module VForth (
 -- * Locations and useable items in the game world
   module VForth.Location
 , module VForth.Item
 -- * Utility functions
 , welcomeMsg
 , isValidUserName
 ) where

import VForth.Location
import VForth.Item
import Data.Text (Text)
import qualified Data.Text as T

-- | The message shown when the application starts
welcomeMsg :: Text
welcomeMsg = "Wake up! It's time to venture forth"

{- |
Checks if the given username is valid. A valid username contains Latin alphabetic
characters only, and is at least three letters long. This is not case-sensitive.
As an example, "Tony" is a valid name

>>> isValidUserName (T.pack "Tony")
True

However names featuring digits, accents or even spaces are not allowed.

>>> map (isValidUserName . T.pack) [" Tony ", "Tony1987",  "Tóni", "Ton", ""]
[False,False,False,False,False]
-}
isValidUserName :: Text -> Bool
isValidUserName uname =
  let
    isAllAlpha = T.all (`elem` ['A'..'Z']) . T.toUpper
  in
    (T.length uname > 3) &&  isAllAlpha uname
