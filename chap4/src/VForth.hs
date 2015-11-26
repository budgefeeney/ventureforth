module VForth (
   module VForth.Location
 , welcomeMsg
 , isValidUserName
 ) where

import VForth.Location
import Data.Char(toUpper)

-- | The message shown when the application starts
welcomeMsg :: String
welcomeMsg = "Wake up! It's time to venture forth"

{- |
Checks if the given username is valid. A valid username contains Latin alphabetic
characters only, and is at least three letters long. This is not case-sensitive.
As an example, "Tony" is a valid name

>>> isValidUserName "Tony"
True

However names featuring digits, accents or even spaces are not allowed.

>>> map isValidUserName [" Tony ", "Tony1987",  "Tóni", "Ton", ""]
[False,False,False,False,False]
-}
isValidUserName :: String -> Bool
isValidUserName uname =
  let
    isAllAlpha = and . map (`elem` ['A'..'Z']) . map toUpper
  in
    (length uname > 3) && (isAllAlpha uname)
