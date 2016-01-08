# Regular Expressions and Haskell

One of the most commonly used ways to validate, search, and amend text is the use of regular expressions. 

Haskell's regular expression support isn't particularly well documented, so as a help for all you guys out there, here is a tutorial on

 * Using regular-expressions in Haskell with the `Text` type
 * Using regular-expressions in Haskell with the `String` type
 
## A Quick Digression into Texts and Strings
For those of you who are unaware, the "standard" Haskell `String` type, being a linked list of boxed characters, offers poor performance for medium to large strings. It is particularly unsuited to the case where most of the text in your appication is mostly read-only.

For this reason two alteratives have been provided. `ByteString` is just a random sequence of unboxed, strict bytes. You can pretend that they're ASCII characters, but since even English speakers are using multi-byte unicode characters now (specifically, emoticons) this will eventually break.

The `Text` type is the solution, it's conceptually the same as a `ByteString` paired with a character-encoding that specifies how the bytes maps to characters. For more on how to use `Text` see the [section on Text in Haskell here](fixme). 

Note that since the `Text` type is not a part of core Haskell, you'll need to specify the [text](https://hackage.haskell.org/package/text/docs/Data-Text.html) package as a dependency of your application.

## Data.Text and Regular Expressions

The [text-icu](http://hackage.haskell.org/package/text-icu) package provides support for advanced text manipulations not covered in the core `text` package, including unicode normalisations, locale-sensitive word & line-breaking, and most importantly for our purposes, regular expresssions. It relies on the C libraries icuuc, icui18n, and icudata, which you'll need to have installed. Linux users can use their package manager. Mac users will probably need to use Homebrew to install the icu4c package, and then direct Cabal to the libraries' locations:

```
brew install icu4c
cabal install --only-dependencies \
	--extra-include-dirs=/usr/local/opt/icu4c/include \
	--extra-lib-dirs=/usr/local/opt/icu4c/lib
``` 

Text-ICU's [regular-expression documentation](http://hackage.haskell.org/package/text-icu-0.7.0.1/docs/Data-Text-ICU.html#g:10) is pretty good. It all relies on a pair of regex constructors

```
regex  :: [MatchOption] -> Text -> Regex
regex' :: [MatchOption] -> Text -> Either ParseError Regex
```
and a pair of finding functions

```
find    :: Regex -> Text -> Maybe Match
findAll :: Regex -> Text -> [Match]
```

The available match-options includes the usual things like case-insensitivity, mult-line support, and line-spanning dot support. As you might expect, `regex` will throw an error if you supply an invalid regex, so if you're accepting unverified input (a risky thing to do) you'll likely prefer `regex'`. 

Once you have a match, you can extract the capture groups using

```
groupCount :: Regular r => r -> Int
group      :: Int -> Match -> Maybe Text
```

where both compiled `Regex` values and `Match` values are instances of `Regular`.

You can use template Haskell (a sort of macro-system / preprocessor for Haskell) to specify regular expressions at compile time. Here's a simple example

```
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ICU hiding (compare)
import Data.Maybe (fromJust)
import Comtrol.Monad (mapM_)

compileTimeRegex :: Regex
compileTimeRegex  = [re|^[a-zA-Z0-9,.:;£$\-!?&()' ]+$|]

-- Note that this is more robust, e.g. it include a Turkish
-- upper-case 'i' (which in Turkish is dotted)
runTimeRegex :: Regex
runTimeRegex = regex [CaseInsensitive] "^[a-z0-9,.:;£$\-!?&()' ]+$"

printMatches :: Regex -> Text -> IO ()
printMatches re text =
	let
		matches = finalAll re text
		matchTexts = map (fromJust . group 0) matches
	in
	mapM_ putStrLn matchTexts
		
```

The best definition of the ICU regular-expression syntax is the from the Java wrapper around the same library, the [java.util.Pattern class](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html), though there are a few differences, the main one being the unicode character classes do no have the "Is" prefix, so the Java class `\p{IsAlphabetic}` is called `\p{Alphabetic}` in ICU. The formal [ICU documentation is here](http://userguide.icu-project.org/strings/regexp)


# String and Regular Expressions

The original Haskell regular-expression support for Strings is rather complex, and not particularly well documented. The first challenge you have is to choose a regular expression engine from one of the [ten available](https://wiki.haskell.org/Regular_expressions#regex-tdfa).

While the [PCRE engine](https://hackage.haskell.org/package/regex-pcre) might seem to be the obvious choice, it has some pecularities due to the overhead of converting Haskell Strings to C Strings on each call. Instead, the best default choice for an engine is the [TDFA engine](http://hackage.haskell.org/package/regex-tdfa). This uses the POSIX syntax, with a [few GNU extensions](https://hackage.haskell.org/package/regex-tdfa-1.2.1/docs/Text-Regex-TDFA.html). POSIX is very similar to PCRE, though it is greedy where PCRE is lazy.

So with that in mind, you'll likely want to add the following two dependencies to your Cabal file

```
regex-base >= 0.93.1
regex-tdfa >= 1.2.1
```

TDFA is built around a Perl style `=~` operator, but since it uses a lot of type tricks to overload this, its [documentation](https://hackage.haskell.org/package/regex-tdfa-1.2.1/docs/Text-Regex-TDFA.html) is not very useful. Here are a few examples that should make things clearer:

```
import Text.Regex.TDFA

mystr = "FooBarBazFud"

mystr =~ "F(oo|ud)" :: Bool 
  == True

mystr =~ "F(oo|ud)" :: String 
  == "Foo"

mystr =~ "F(oo|ud)" :: (MatchOffset, MatchLength)
  == (0, 3)

getAllTextMatches (mystr =~ "F(oo|ud)") :: [String]
  == ["Foo", "Fud"]
    
getAllMatches (mystr =~ "F(oo|ud)") :: [(MatchOffset, MatchLength)]
  == [ (0, 3), (9, 3) ]  
```

Note there are two different functions: `getAllTextMatches` and `getAllMatches`.

Regular expressions are, of course, case-insensitive, so

```
mystr =~ "f(oo|ud)" :: Bool == False
```

To make a case-insensitive regex you need to explicity construct a regex value using the `makeRegexOpts` function and two data-types: [CompOption and ExecOption](https://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA-Common.html). A useful shortcut is to use the `defaultCompOpt` value as the basis for your new `CompOption` value and likewise for `ExecOption`

```
compOpts = defaultCompOpt { caseSensitive = True }
execOpts = defaultExecOpt { captureGroups = True }

regex = makeRegexOpts compOpts execOpts "f(oo|ud)"

match regex mystr :: Bool 
  == true
  
getAllTextMatches (match regex mystr) :: [String]
  == ["Foo","Fud"]  
  
-- and so on
```

Note that if you provide an invalid regular-expression pattern to `makeRegexOpts`, it will return without any complaint, but the first call to `match` will throw an exception. If you're taking an untrusted text as a regular expression (itself a very dangerous business), you may prefer instead to put everything in a Monad, use the `makeRegexOptsM` function and use `MonadError` to handle the errors, as described on the [RegexMaker hackage entry](http://hackage.haskell.org/package/regex-base-0.93.2/docs/Text-Regex-Base-RegexLike.html). The simplest approach is just to use the `Either` monad:

```
-- Makes Either an instance of MonadError
import Control.Monad.Except

regex = case makeRegexOptsM compOpts execOpts "f(oo|ud)" of 
			Left err -> error ("Invalid regex: " <> err)
			Right re -> re
```




