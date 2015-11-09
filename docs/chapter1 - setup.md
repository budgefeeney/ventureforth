# Venturing Forth

Over the last year, I've decided to learn a new languages, and I settled on Haskell as it requires not only learning a new syntax, but also a new way of structuring programs, which I found intriguing. There two most popular online resources, [Learn You a Haskell](http://learnyouahaskell.com/chapters) and the rather outdated [Real World Haskell](http://book.realworldhaskell.org/read/). Both do a good job of describing the language, but neither goes through the process of designing an functional applications, nor how to layout a "proper" Haskell project with unit-testing and code-coverage.

In the end, I found the best way of learning functional design was exploring F# tutorials, notably [this series](http://fsharpforfunandprofit.com/series/annotated-walkthroughs.html) on designing functional F# applications,  specifically [the "enterprise" tic-tac-toe example](http://fsharpforfunandprofit.com/posts/enterprise-tic-tac-toe/).

To help other Haskellers, I have therefore decided to create a full, end-to-end application dealing with all the awkward aspects a real Haskell program must deal with: project layout, dependency management, unit-testing, I/O, configuration, and state.

To keep the focus on the core language, I've chosen to develop a [text adventure game](https://en.wikipedia.org/wiki/Interactive_fiction). This is a pretty prehistoric class of app, but it is a bit more fun than Fibonnacci series. The application will load the game world, load the user's saved state, then print a description of the room they're in and what items they're carrying. The user then types in commands like "move north" or "use wallet" or "use bag with clothes" to interact with that world. The particular game features a hungover student, an English town, and a bag of dirty laundry that needs cleaning.

This game is in fact a port of a game written as part of an Amstrad CPC BASIC tutorial published in issues [93](https://archive.org/details/amstrad-action-093), [94](https://archive.org/details/amstrad-action-094), [95](https://archive.org/details/amstrad-action-095) and [96](https://archive.org/details/amstrad-action-096) of [Amstrad Action](https://en.wikipedia.org/wiki/Amstrad_Action) magazine back in 1993.


In this first part of the tutorial we'll

 1. Create a full project skeleton
 2. Create a single simple type representing a location in our game
 3. Add a unit test for it
 4. Use cabal to run unit-tests, evaluate coverage, and generate code-documentation

You'll need to have a fully functioning Haskell development environment setup with `cabal`, GHC 7.10 or later and ideally some sort of decent IDE. For instructions on how to set this up look at this [previous post](fixme).

This first part is unfortunately long on setup and short on code, the rest should be a lot more enjoyable and code-centric.


# Starting a new Haskell Project

As Venture Forth will be a "proper" application it will have be comprised of the following parts

 * A library with all the core testable logic
 * An application using that library - typically this is just a single Main.hs project
 * A set of unit tests testing the library only.
 	* We will also want to record both test coverage and documentation coverage.
 * A set of benchmarks evaluating performance
 
We'll be using `cabal` to handle almost all of this for us. However to get us started quickly, we'll use `stack` to create a template standalone application project. Stack is capable of much more than this, but to keep things simple, we won't be using it.

Lets assume you're going to be working in `$HOME/Workspace/vforth`. Open a terminal and type the following


```
cd $HOME/Workspace
stack init vforth chrisdone
```

This creates a folder called `vforth` and within it creates a project according to the "chrisdone" application template. You should edit the `vforth.cabal` file to fit your needs. It's mostly self-explanatory: the only subtlety is the difference between "synopsis" and "description". The "synopsis" is a single-sentence summary of your project, while "description" is a paragraph or two descriping it in depth. In the template you're just referred to the `README.md` file, which you should also edit, as well as the licence file.




### Where to put the code
For libraries, you are expected to create package names which fit directly into the [existing namespace](https://wiki.haskell.org/Hierarchical_module_names), the [Lens project](https://github.com/ekmett/lens) is a good example.

However for applications, it's better to have a top-level package corresponding to your application name. [XMonad](https://github.com/xmonad/xmonad) is an example of one such modern Haskell application.

Thus we'll create a top-level package corresponding to our application which we'll name "VenForth". Within that, the preference for Haskell is generally to keep hierarchies flat, though we will go slightly deep with separate `Entity`, `Parser` etc packages.

For this skeleton, we'll just create a very simple `Location` type representing a place in the game.

So in `src/VenForth/Entites` create a file `Location.hs` with the contents

```
module Location (
  Location(..)
  )
  where
  
  data Location = Location {
      title :: String
  	, description :: String
  	}
  	
  instance Show Location where
    show l = 
    	title l ++ "\n"
    	++ replicate (length $ title l) '-' ++ "\n"
    	++ )description l
    	
```

We'll flesh this out in the next post. For the time being, we've create a basic `Location` datatype, and implemented the only typeclass we currently care about, `Show`.



# Futher Reading:
Resources on how to create a Haskell project include [this example of starting a Haskell project](https://howistart.org/posts/haskell/1) and the [Holy Haskell Project Starter](http://yannesposito.com/Scratch/en/blog/Holy-Haskell-Starter/). 



-----
