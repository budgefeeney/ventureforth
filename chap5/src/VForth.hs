{-# LANGUAGE OverloadedStrings #-}
module VForth (
   module VForth.Location
 , module VForth.Item
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
