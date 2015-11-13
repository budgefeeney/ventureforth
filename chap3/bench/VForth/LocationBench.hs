module VForth.LocationBench where

import Criterion.Main
import VForth

benchmarks :: [Benchmark]
benchmarks = [ bench "show" (whnf show l) ]
  where l = Location {
                title="My Title"
              , description = "This is the description."
              }
