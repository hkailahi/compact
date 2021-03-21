{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

-- | Entropy is the miminal length, in bits, needed to "codes" needed to uniquely identify items of some source
--  set.
module Ch2.Entropy where

import qualified Control.Monad.Bayes.Class as Bayes ()
import qualified Control.Monad.Bayes.Enumerator as Bayes
import qualified Data.Algorithms.KMP as KMP
import qualified Data.Map.Strict as SM (foldMapWithKey)
import Relude
import Relude.Extra.Map (DynamicMap (insert))

----------------------------------------------------------------------------------------------------

-- ** Bitwise Encodings

-- | Minimum __total__ length of identifier given all codes have the same length (in bits).
--
--   @
--    -- Encoding [1..8] with {1 -> 0b000, 2 -> 0b001, ... , 8 -> 0b111}
--    >>> worstCaseEntropy [1..8]
--    3
--    -- Encoding [1..14] with {1 -> 0b0000 ... 14 -> 0b1101}
--    >>> worstCaseEntropy [1..14]
--    4
--   @
--
-- ==== __More Examples__
--
-- TBD
--
-- ==== __Background__
-- Given a set @U@ with unique, same-length codes assigned to all elements,
-- \(H_{wc}(U)\) denotes the \worst-case entropy\ of @U@.
--
-- \[H_{wc}(U) = log_2 |U| \text{, with base 2 as we're encoding with bits}\]
--
-- ==== __Resources__
--
-- For a video introduction, see Ben Langmead's <https://youtu.be/TepaLzgatAw Entropy and Coding>
-- for encoding playing cards.
-- <<images/LangmeadWorstCaseCards.png>>
worstCaseEntropy :: forall t a. (Foldable t, Ord a) => t a -> Natural
worstCaseEntropy = worstCaseEntropyBase 2

----------------------------------------------------------------------------------------------------

-- | Minimum __average__ length of identifier for items in a "memoryless" source, calculated by
--  incorporating the probability of each symbol occurring in the source.
--
--  \"Memoryless\" means that probability of each item can be represented as an independent random
--  variable, meaning that a random selection of one item has no impact on what item would be chosen
--  in a follow-up random selection.
--
--   @
--     {- Example 0b111000 (3 ones + 2 zeroes = 5 bits) -}
--     >>> shannonEntropy $ Bayes.uniformD [1..6]
--     3
--     >>> shannonEntropy . Bayes.categorical $ V.fromList [1..50]
--     4
--   @
--
-- ==== __More Examples__
--
-- TBD
--
-- ==== __Background__
--
-- \(H(Pr)\) denotes the /Shannon entropy/ of the /probability distribution/
-- \(Pr : U \rightarrow [0.0, 1.0]\), which we
--
-- \[H(Pr) = \sum_{u \epsilon U} Pr(u) * log \frac{1}{Pr(u)}\]
--
--  The measure \(H(Pr)\) can be interpreted as how many bits of information are contained in each
--  element emitted by the source. The more biased the probability distribution, the more
--  “predictable,” or the less “surprising,” the output of the source is, and the less information
--  is carried by its elements. In particular, if one probability tends to 1.0 and all the others
--  tend to 0.0, then \(H(Pr)\) tends to 0.
--
--  When outcomes are equiprobable, the Shannon entropy fully degrades and is equal to the worst case
--  entropy
--
-- ==== __Resources__
--
-- For a video introduction, see Ben Langmead's <https://youtu.be/TepaLzgatAw?t=505 Entropy and Coding>
-- for analyzing varying length morse codes.
-- <<images/LangmeadShannonMorse.png>>
shannonEntropy :: Ord a => Bayes.Enumerator a -> Natural
shannonEntropy = shannonEntropyBase 2

----------------------------------------------------------------------------------------------------

data EntropyErr
  = Wtf

-- | Assume bernoulli distribution of n/m
--
--  @
--    -- Example 0b111000 (3 ones + 2 zeroes = 5 bits)
--    >>> bitFromBool = map (first (\case False -> 0; True -> 1))
--    >>> bitFromBool . `Bayes.enumerate` $ `Bayes.bernoulli` $ 3 / 5
--    [(0,0.4),(1,0.6)]
--  @
--
-- ==== __More Examples__
--
-- TBD
--
-- ==== __Background__
--
-- Assume bernoulli distribution of n/m
--
-- ==== __Resources__
--
-- For a video introduction, see Ben Langmead's
-- <https://www.youtube.com/watch?v=TepaLzgatAw&t=1570s Entropy and Coding> for encoding
-- a string when we don't have a model, but we can __empirically__ count the zeros and ones in our
-- string to train one.
--
-- "Huffman coding" is a common zero-order compressor. For example, a possible huffman coding of
-- @"abracadabra"@ could be:
-- <<images/LangmeadEmpiricalHuffman.png>>
empiricalEntropy ::
  -- | Occurrences
  Natural ->
  -- | Total Size
  Natural ->
  Natural
empiricalEntropy = empiricalEntropyBase 2

-- | Synonym for `empiricalEntropy`
zeroOrderEntropy ::
  -- | Occurrences
  Natural ->
  -- | Total Size
  Natural ->
  Natural
zeroOrderEntropy = empiricalEntropy

----------------------------------------------------------------------------------------------------

-- | Weighted sum over all contexts (left or right neighbors) of the zero order
-- empirical entropy of symbols having that context.
--
-- FIXME Underflowing asdofnaspobnoainroairnvaeojn
--
-- @
--   >>> higherOrderEntropy 1 [1,3,1,2,3,1,2,3]
--   *** Exception: arithmetic underflow
--   >>> higherOrderEntropy 2 [1,3,1,2,3,1,2,3]
--   *** Exception: arithmetic underflow
--   >>> higherOrderEntropy 3 [1,3,1,2,3,1,2,3]
--   *** Exception: arithmetic underflow
-- @
--
-- ==== __More Examples__
--
-- TBD
--
-- ==== __Background__
--
-- \(H_k\) of length-@n@ string @S@ is a weighted sum over all contexts of the zero order
-- empirical entropy of symbols having that context.
--
-- \[H_k(S) = \sum_{t \ \epsilon \sum^k} \frac{S_t}{n} * H_0(S_t) \text{, where k > 0}\]
--
-- ==== __Resources__
--
-- For a video introduction, see Ben Langmead's <https://youtu.be/nNqPTcbX1s4 Higher order empirical entropy>
-- <<images/LangmeadHOLeftContext.png>> <<images/LangmeadHOLeftContextTree.png>>
higherOrderEntropy :: forall t a. (Foldable t, Ord a) => Natural -> t a -> Natural
higherOrderEntropy k (toList -> str) =
  ceiling
    . getSum
    $ SM.foldMapWithKey weighCtx ctxOccs
  where
    -- See https://stackoverflow.com/a/21288092/8540453, also https://stackoverflow.com/questions/21775378/
    subsequencesOfSize :: Natural -> [a] -> [[a]]
    subsequencesOfSize (fromIntegral -> n) full@(length -> l)
      | n > l = []
      | otherwise = catMaybes . sequenceA $ subsequencesBySize full !!? (l - n)
      where
        subsequencesBySize = \case
          [] -> [[[]]]
          (x : xs) ->
            let next = subsequencesBySize xs
             in zipWith (++) ([] : next) (map (map (x :)) next ++ [[]])

    kOrderCtxs :: Set [a]
    kOrderCtxs = fromList $ subsequencesOfSize k str

    insertObsCtx ::
      (ctx ~ occ, ctx ~ [a]) =>
      ctx ->
      Map ctx occ ->
      Map ctx occ
    insertObsCtx ctx =
      let matchIxs = KMP.match (KMP.build ctx) str
          leftCtxIxs = filter (> 0) $ map (\x -> x - 1) matchIxs
          leftOccs = mapMaybe (str !!?) leftCtxIxs
       in insert ctx leftOccs

    ctxOccs :: Map [a] [a]
    ctxOccs = foldr insertObsCtx mempty kOrderCtxs

    -- alphabet :: Set a
    -- alphabet = fromList $ toList str

    weighCtx :: [a] -> [a] -> Sum Double
    weighCtx _ leftOcc =
      Sum $
        fromIntegral (length leftOcc) / fromIntegral (length str)
          * fromIntegral (worstCaseEntropy leftOcc)

----------------------------------------------------------------------------------------------------

-- ** k-Base Encodings

worstCaseEntropyBase :: forall t a. (Foldable t, Ord a) => Double -> t a -> Natural
worstCaseEntropyBase k = ceiling . logBase k . fromIntegral . setLength
  where
    setLength = length . fromList @(Set a) . toList -- FIXME Use single pass

shannonEntropyBase :: Ord a => Double -> Bayes.Enumerator a -> Natural
shannonEntropyBase k distribution =
  ceiling
    . getSum
    . foldMap (Sum . shannon . snd)
    $ Bayes.enumerate distribution
  where
    shannon :: Double -> Double
    shannon probX = probX * logBase k (recip probX)

empiricalEntropyBase ::
  -- | Base
  Double ->
  -- | Occurrences
  Natural ->
  -- | Total Size
  Natural ->
  Natural
empiricalEntropyBase k occ totalSize
  | occ > totalSize = error "wtf"
  | otherwise =
    ceiling $
      ratioOnes * logBase k (recip ratioOnes)
        + ratioZeros * logBase k (recip ratioZeros)
  where
    (./) :: Integral a => a -> a -> Double
    (./) a b = fromIntegral a / fromIntegral b
    ratioOnes :: Double
    ratioOnes = occ ./ totalSize
    ratioZeros = (totalSize - occ) ./ totalSize

{-
TODO Derivable and specialized entropy calc functions
  * List
    * One pass set length using intermediate set
  * Set
    * No intermediate value, just `length`
  * BoundedEnum
    * getEntropy . maxBound
      * skip making intermediate collection
    * `enumEntropy`??
  * Generic
    * Entropy of first level constructors
    * Deeper entropy of inner products
    * `gentropy`?? `gShannon`??
-}
