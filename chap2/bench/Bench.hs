
import Criterion.Main
import qualified VForth.LocationBench as LocationBench

main :: IO ()
main = defaultMain [
  bgroup "Location" LocationBench.benchmarks
  ]
