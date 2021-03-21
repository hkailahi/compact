import qualified Ch2.CodingSpec as CodingSpec
import qualified Ch2.EntropySpec as EntropySpec
import Relude
import Test.Syd

main :: IO ()
main = sydTest $ do
  CodingSpec.spec
  EntropySpec.spec
