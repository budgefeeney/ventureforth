
import Criterion.Main
import qualified VForth.LocationBench as LocationBench
import qualified VForth.TextBench as TextBench

main :: IO ()
main = defaultMain [
    bgroup "Location" LocationBench.benchmarks
  , bgroup "Combine" TextBench.combineBenchmarks
  , bgroup "Length of Combine" TextBench.lengthCombineBenchmarks
  ]
