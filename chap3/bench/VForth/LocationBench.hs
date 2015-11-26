module VForth.LocationBench where

import Criterion.Main
import VForth

benchmarks :: [Benchmark]
benchmarks = [ bench "length.show" (whnf (length . show) l) ]
  where l = Location {
    title = "Your Bedroom"
    , description = unlines [
      "You're in your bedroom. It's an utterly disgusting tip of a place. ",
      "Dirty coffee mugs everywhere, bits of computer and motorbike all ",
      "over the floor. It's an outrage. You can leave by going north, and ",
      "maybe you should."
      ]
    }
