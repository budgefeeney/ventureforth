import Test.Hspec
import qualified VForth.LocationSpec as LocationSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Location" LocationSpec.spec
