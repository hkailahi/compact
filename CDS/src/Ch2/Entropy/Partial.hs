{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

-- | Entropy is the miminal length, in bits, needed to "codes" needed to uniquely identify items of some source
--  set.
module Ch2.Entropy.Partial where

import Ch2.Entropy
import qualified Control.Monad.Bayes.Enumerator as Bayes
import Relude

unsafeShannonEntropy :: Ord a => Bayes.Enumerator a -> Natural
unsafeShannonEntropy = shannonEntropy

unsafeEmpiricalEntropy :: Natural -> Natural -> Natural
unsafeEmpiricalEntropy = empiricalEntropy

unsafeHigherOrderEntropy :: (Foldable t, Ord a) => Natural -> t a -> Natural
unsafeHigherOrderEntropy = higherOrderEntropy
