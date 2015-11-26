{-LANGUAGE BangPatterns -}
module VForth.TextBench where

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as Bldr
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Criterion.Main

combineStr :: (String, String) -> String
combineStr (title, desc) = title ++ separator ++ desc
  where separator = "\n" ++ (replicate tLen '-') ++ "\n"
        tLen      = length title

combineTextSimple :: (T.Text, T.Text) -> T.Text
combineTextSimple (title, desc) = title <> separator <> desc
  where separator = newline <> (T.replicate tLen dash) <> newline
        tLen      = T.length title
        newline   = T.pack "\n"
        dash      = T.pack "-"

combineTextBuilder :: (T.Text, T.Text) -> T.Text
combineTextBuilder (title, desc) =
  LT.toStrict
  $ Bldr.toLazyText
  $ Bldr.fromText title <> sep <> Bldr.fromText desc
  where
    endl     = Bldr.singleton '\n'
    titleLen = fromIntegral (T.length title)
    dash     = LT.pack "-"
    dashes   = Bldr.fromLazyText $ LT.replicate titleLen dash
    sep      = endl <> dashes <> endl


combineBStringSimple :: (BS.ByteString, BS.ByteString) -> BS.ByteString
combineBStringSimple (title, desc) = title <> separator <> desc
  where separator = newline <> (BS.replicate tLen '-') <> newline
        tLen      = BS.length title
        newline   = BS.pack "\n"

combineBenchmarks :: [Benchmark]
combineBenchmarks = [
  -- Strings
    bench "String   : Short" (whnf combineStr shortStr)
  , bench "String   : Med  " (whnf combineStr mediumStr)
  , bench "String   : Long " (whnf combineStr longStr)

  -- Text, simple
  , bench "Text     : Short" (whnf combineTextSimple shortText)
  , bench "Text     : Med  " (whnf combineTextSimple mediumText)
  , bench "Text     : Long " (whnf combineTextSimple longText)

  -- Text, builder
  , bench "Text/Bld : Short" (whnf combineTextBuilder shortText)
  , bench "Text/Bld : Med  " (whnf combineTextBuilder mediumText)
  , bench "Text/Bld : Long " (whnf combineTextBuilder longText)

  -- ByteString, simple
  , bench "BString  : Short" (whnf combineBStringSimple shortBStr)
  , bench "BString  : Med  " (whnf combineBStringSimple mediumBStr)
  , bench "BString  : Long " (whnf combineBStringSimple longBStr)

  ]

lengthCombineBenchmarks :: [Benchmark]
lengthCombineBenchmarks = [
  -- Strings
    bench "String   : Short" (whnf (length . combineStr) shortStr)
  , bench "String   : Med  " (whnf (length . combineStr) mediumStr)
  , bench "String   : Long " (whnf (length . combineStr) longStr)

  -- Text, simple
  , bench "Text     : Short" (whnf (T.length . combineTextSimple) shortText)
  , bench "Text     : Med  " (whnf (T.length . combineTextSimple) mediumText)
  , bench "Text     : Long " (whnf (T.length . combineTextSimple) longText)

  -- Text, builder
  , bench "Text/Bld : Short" (whnf (T.length . combineTextBuilder) shortText)
  , bench "Text/Bld : Med  " (whnf (T.length . combineTextBuilder) mediumText)
  , bench "Text/Bld : Long " (whnf (T.length . combineTextBuilder) longText)

  -- ByteString, simple
  , bench "BString  : Short" (whnf (BS.length . combineBStringSimple) shortBStr)
  , bench "BString  : Med  " (whnf (BS.length . combineBStringSimple) mediumBStr)
  , bench "BString  : Long " (whnf (BS.length . combineBStringSimple) longBStr)

  ]

shortTitle :: String
shortTitle = "Greeting"
shortDesc  :: String
shortDesc  = "Hello, world!"
shortStr  :: (String, String)
shortStr   = (shortTitle, shortDesc)
shortText :: (T.Text, T.Text)
shortText  = (T.pack shortTitle, T.pack shortDesc)
shortBStr :: (BS.ByteString, BS.ByteString)
shortBStr  = (BS.pack shortTitle, BS.pack shortDesc)

mediumTitle :: String
mediumTitle = "Chapter 12: Voyage into the Unknown"
mediumDesc :: String
mediumDesc  = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
mediumStr  :: (String, String)
mediumStr   = (mediumTitle, mediumDesc)
mediumText :: (T.Text, T.Text)
mediumText  = (T.pack mediumTitle, T.pack mediumDesc)
mediumBStr :: (BS.ByteString, BS.ByteString)
mediumBStr  = (BS.pack mediumTitle, BS.pack mediumDesc)

longTitle :: String
longTitle = mediumDesc
longDesc  :: String
longDesc  = concat $ replicate 10 mediumDesc
longStr   :: (String, String)
longStr   = (longTitle, longDesc)
longText  :: (T.Text, T.Text)
longText  = (T.pack longTitle, T.pack longDesc)
longBStr  :: (BS.ByteString, BS.ByteString)
longBStr  = (BS.pack longTitle, BS.pack longDesc)
