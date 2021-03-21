-- | Entropy is the miminal length, in bits, needed to "codes" needed to uniquely identify items of some source
--  set.
module Ch2.EntropySpec where

import Relude
import Test.Syd

-- import Ch2.Entropy

spec :: Spec
spec = do
  worstCaseEntropySpec
  shannonEntropySpec
  empiricalEntropySpec
  higherOrderEntropySpec

worstCaseEntropySpec :: Spec
worstCaseEntropySpec = describe "worstCaseEntropy" $ do
  it "blah" $ do
    1 `shouldBe` 1

shannonEntropySpec :: Spec
shannonEntropySpec = describe "shannonEntropy" $ do
  it "blah" $ do
    1 `shouldBe` 1

empiricalEntropySpec :: Spec
empiricalEntropySpec = describe "empiricalEntropy" $ do
  it "blah" $ do
    1 `shouldBe` 1

higherOrderEntropySpec :: Spec
higherOrderEntropySpec = describe "higherOrderEntropy" $ do
  it "blah" $ do
    1 `shouldBe` 1
