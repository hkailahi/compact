-- | Entropy is the miminal length, in bits, needed to "codes" needed to uniquely identify items of some source
--  set.
module Ch2.Entropy where

import qualified Control.Monad.Bayes.Class as Bayes
import qualified Control.Monad.Bayes.Enumerator as Bayes
import qualified Data.Algorithms.KMP as KMP
import qualified Data.Map.Strict as SM (foldMapWithKey)
import qualified Data.Set as Set
-- import qualified Data.MultiSet as MultiSet
-- import qualified Data.Vector as V
import Relude
import Relude.Extra.Map (DynamicMap (insert))
import Relude.Unsafe (fromJust)

newtype Entropy (base :: Nat) = Entropy
  {unEntropy :: Double}
  deriving (Eq, Show, Ord)
  deriving (Num) via Double

minCountBitRep :: Entropy base -> Natural
minCountBitRep = ceiling . unEntropy

uncompressedBitRep :: Natural -> Natural
uncompressedBitRep 0 = 1
uncompressedBitRep 1 = 1
uncompressedBitRep n = ceiling . logBase 2 $ fromIntegral n

-- instance Pretty Entropy where
--    prettyShow = minCountBitRep

----------------------------------------------------------------------------------------------------

-- ** Bitwise Encodings

-- *** Same Length Encodings

-- **** Memoryless Encodings

-- $ In a /memoryless/ encoding, the probability of each symbol is independent of previous
-- occurrences. The probability distribution of symbols from the input source is known prior to
-- encoding it, so work or space needs to be allocated before encoding.
--
-- Put another way, /memoryless/ means that probability of each item can be represented as an
-- independent random variable, meaning that a random selection of one item has no impact on what
-- item would be chosen in a follow-up random selection.

-- | Minimum __total__ length of identifier given all codes have the same length (in bits).
--
-- ==== __Background__
--
-- Given a set @U@ with unique, same-length codes assigned to all elements,
-- \(H_{wc}(U)\) denotes the \worst-case entropy\ of @U@.
--
-- \[H_{wc}(U) = log_2 |U| \text{, with base 2 as we're encoding with bits}\]
--
-- ==== __Examples__
--
-- ===== Encoding 7 distinct characters, like "abcdefg" or [11..17]
--
-- Largest code needs 3 bits:
--
-- +-------+-------+-------+-------+-------+-------+-------+
-- | a     | b     | c     | d     | e     | f     | g     |
-- +=======+=======+=======+=======+=======+=======+=======+
-- | 0b000 | 0b001 | 0b010 | 0b011 | 0b100 | 0b101 | 0b110 |
-- +-------+-------+-------+-------+-------+-------+-------+
--
--   @
--    >>> worstCaseEntropy ("abcdefg" :: [Char])
--    Entropy {unEntropy = 2.807354922057604}
--   @
--
-- ===== Encoding 5 distinct characters, like "lmnop" or [101..105]
--
-- Largest code needs 3 bits:
--
-- +-------+-------+-------+-------+-------+
-- | 101   | 102   | 103   | 104   | 105   |
-- +=======+=======+=======+=======+=======+
-- | 0b000 | 0b001 | 0b010 | 0b011 | 0b100 |
-- +-------+-------+-------+-------+-------+
--
--   @
--    >>> worstCaseEntropy [101..105]
--    Entropy {unEntropy = 2.321928094887362}
--   @
--
-- ==== __Resources__
--
-- For a video introduction, see Ben Langmead's <https://youtu.be/TepaLzgatAw Entropy and Coding>
-- for encoding playing cards.
-- <<images/LangmeadWorstCaseCards.png>>
worstCaseEntropy :: forall t a. (Foldable t, Ord a) => t a -> Entropy 2
worstCaseEntropy = worstCaseEntropyBase

----------------------------------------------------------------------------------------------------

-- *** Variable Length Encodings

-- $ These can be converted to same length encodings by taking the 'ceiling'. In some cases, this will
-- still be less than the worst case entropy, though additional storage outside the codes will be
-- required.

-- **** Memoryless Encodings

-- $ In a /memoryless/ encoding, the probability of each symbol is independent of previous
-- occurrences. The probability distribution of symbols from the input source is known prior to
-- encoding it, so work or space needs to be allocated before encoding.
--
-- Put another way, /memoryless/ means that probability of each item can be represented as an
-- independent random variable, meaning that a random selection of one item has no impact on what
-- item would be chosen in a follow-up random selection.

-- | Minimum __average__ length of identifier for items in a "memoryless" source, calculated by
--  incorporating the probability of each symbol occurring in the source.
--
-- The entropy of independent events are the sum of their entropies.
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
-- ==== __Examples__
--
-- ===== 6 equally likely, distinct values, like @abcdef@ or @[5,10,15,20,25,30]@
--
--   @
--     >>> shannonEntropy $ Bayes.uniformD [1..6]
--     Entropy {unEntropy = 2.5849625007211565} -- 3 bits
--   @
--
-- ===== Distinct pair of equally likely values, like @0b10@, @0b11001001@, @aabb@, etc
--
--  @
--    >>> shannonEntropy $ Bayes.bernoulli 0.5
--    Entropy {unEntropy = 1.0}
--    >>> worstEntropy [1,0]
--  @
--
-- ===== Distinct pair of 9:1 ratio of values, such as `0b1111111110` or `"abbbbbbbbb"`
--
--  @
--   >>> shannonEntropy $ Bayes.bernoulli 0.1
--   Entropy {unEntropy = 0.46899559358928133}
--   >>> shannonEntropy $ Bayes.bernoulli 0.9
--   Entropy {unEntropy = 0.4689955935892812}
--  @
--
-- ==== __Resources__
--
-- For a video introduction, see Ben Langmead's <https://youtu.be/TepaLzgatAw?t=505 Entropy and Coding>
-- for analyzing varying length morse codes.
-- <<images/LangmeadShannonMorse.png>>
shannonEntropy :: Ord a => Bayes.Enumerator a -> Entropy 2
shannonEntropy = shannonEntropyBase

-- | Shannon entropy for bitwise encoding. Synyonm for `shannonEntropy` above, which assumes bitwise
-- encoding.
binaryEntropy :: Ord a => Bayes.Enumerator a -> Entropy 2
binaryEntropy = shannonEntropyBase

----------------------------------------------------------------------------------------------------

-- **** Empirical Encodings

-- $ Require memory outside of codes, usually to build up probability distribution from
-- observations.
--
-- By consuming the entire input source and recording all occurrences, we can build up a probability
-- distribution to use as input to the same \memoryless\ encodings shown above.

-- | TBD
--
-- ==== __Background__
--
-- The zero-order empirical entropy of a string @S[1, n]@, where each symbol @s@ appears \(n_s\)
-- times in @S@, is also defined in terms of the /Shannon entropy/ of its observed probabilities.
--
-- \[ H_0(S) = H( \langle \frac{n_1}{n}, ..., \frac{n_{\sigma}}{n} \rangle ) = \sum_{1 \leq \ s \ \leq \sigma} \frac{n_s}{n}log\frac{n}{n_s} \]
--
-- ==== __Examples__
--
-- ===== Encoding string with skewed occurrences of characters
--
-- Via @foldr MultiSet.insert mempty ("abracadabra-alakazam" :: [Char])@:
--
-- +---+---+---+---+---+---+---+---+---+---+
-- | a | b | r | c | d | k | l | m | z | - |
-- +===+===+===+===+===+===+===+===+===+===+
-- | 9 | 2 | 2 | 1 | 1 | 1 | 1 | 1 | 2 | 1 |
-- +---+---+---+---+---+---+---+---+---+---+
--
--  @
--    >>> empiricalEntropy ("abracadabra-alakazam" :: [Char])
--    Entropy {unEntropy = 2.6954618442383222}
--  @
--
-- which is saves a whole bit for each code:
--
--  @
--    >>> worstCaseEntropy ("abracadabra-alakazam" :: [Char])
--    Entropy {unEntropy = 3.3219280948873626}
--  @
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
empiricalEntropy :: forall t a. (Foldable t, Ord a) => t a -> Entropy 2
empiricalEntropy = empiricalEntropyBase

-- | Synonym for `empiricalEntropy`
zeroOrderEntropy :: forall t a. (Foldable t, Ord a) => t a -> Entropy 2
zeroOrderEntropy = empiricalEntropy

----------------------------------------------------------------------------------------------------

-- | Weighted sum over all contexts (left or right neighbors) of the zero order
-- empirical entropy of symbols having that context.
--
-- ==== __Background__
--
-- \(H_k\) of length-@n@ string @S@ is a weighted sum over all contexts of the zero order
-- empirical entropy of symbols having that context.
--
-- \[H_k(S) = \sum_{t \ \epsilon \sum^k} \frac{S_t}{n} * H_0(S_t) \text{, where k > 0}\]
--
-- ==== __Examples__
--
-- ===== Encoding "abracadabra"
--
-- Table via @'ctxOccsL' "abracadabra" 1@:
--
-- +------------------+------------------+------------------+------------------+------------------+
-- | \(S_a\)          | \(S_b\)          | \(S_r\)          | \(S_c\)          | \(S_d\)          |
-- +==================+==================+==================+==================+==================+
-- | @bcdb@           | @rr@             | @aa@             | @a@              | @a@              |
-- +------------------+------------------+------------------+------------------+------------------+
-- | \(\frac{4}{11}\) | \(\frac{2}{11}\) | \(\frac{2}{11}\) | \(\frac{1}{11}\) | \(\frac{1}{11}\) |
-- +------------------+------------------+------------------+------------------+------------------+
--
-- \[
--   H_{k=1}(abracadabra) = \frac{4}{11}H_0(bcdb) + \frac{2}{11}H_0(rr) + \frac{2}{11}H_0(aa) + \frac{2}{11}H_0(a) + \frac{1}{11}H_0(a)
--   \\ = \frac{4}{11}*1.5 + \frac{2}{11}*0 + \frac{2}{11}*0 + \frac{1}{11}*0 + \frac{1}{11}*0
--   \\ = 0.5454545454545454
-- \]
--
--  @
--    >>> higherOrderEntropy @2 1 ("abracadabra" :: String)
--    Entropy {unEntropy = 0.5454545454545454}
--  @
--
-- This is a large saving over:
--
--  @
--    >>> empiricalEntropy ("abracadabra" :: String)
--    Entropy {unEntropy = 2.0403733936884962}
--    >>> worstCaseEntropy ("abracadabra" :: String)
--    Entropy {unEntropy = 2.321928094887362}
--  @
--
-- ===== Encoding "13123123"
--
-- Table via @'ctxOccsL' [1,3,1,2,3,1,2,3] 1@:
--
-- +-----------------+-----------------+-----------------+
-- | \(S_1\)         | \(S_2\)         | \(S_3\)         |
-- +=================+=================+=================+
-- | @322@           | @33@            | @11@            |
-- +-----------------+-----------------+-----------------+
-- | \(\frac{3}{8}\) | \(\frac{2}{8}\) | \(\frac{2}{8}\) |
-- +-----------------+-----------------+-----------------+
--
-- \[
--   H_{k=1}(13123123) = \frac{3}{8}H_0(322) + \frac{2}{8}H_0(33) + \frac{2}{8}H_0(11)
--   \\ = \frac{4}{8}*0.9182958340544896 + \frac{2}{11}*0 + \frac{2}{11}*0 + \frac{1}{11}*0 + \frac{1}{11}*0
--   \\ = 0.3443609377704336
-- \]
--
--  @
--    >>> higherOrderEntropy 1 [1,3,1,2,3,1,2,3]
--    Entropy {unEntropy = 0.3443609377704336}
--  @
--
-- This is a large saving over:
--
--  @
--    >>> empiricalEntropy [1,3,1,2,3,1,2,3]
--    Entropy {unEntropy = 1.561278124459133}
--    >>> worstCaseEntropy [1,3,1,2,3,1,2,3]
--    Entropy {unEntropy = 1.5849625007211563}
--  @
--
-- ==== __Resources__
--
-- For a video introduction, see Ben Langmead's <https://youtu.be/nNqPTcbX1s4 Higher order empirical entropy>
-- <<images/LangmeadHOLeftContext.png>> <<images/LangmeadHOLeftContextTree.png>>
higherOrderEntropy :: forall t a. (Foldable t, Ord a) => Natural -> t a -> Entropy 2
higherOrderEntropy = higherOrderEntropyBase

----------------------------------------------------------------------------------------------------

-- ** Parameterized Base Encodings

worstCaseEntropyBase :: forall base t a. (KnownNat base, Foldable t, Ord a) => t a -> Entropy base
worstCaseEntropyBase xs
  | null xs = Entropy 0.0
  | otherwise = Entropy . logBase (fromIntegral . natVal $ Proxy @base) . fromIntegral $ setLength
  where
    setLength = length . fromList @(Set a) $ toList xs -- FIXME Use single pass

shannonEntropyBase :: forall base a. (KnownNat base, Ord a) => Bayes.Enumerator a -> Entropy base
shannonEntropyBase distribution =
  Entropy
    . getSum
    . foldMap (Sum . shannon . snd)
    $ Bayes.enumerate distribution
  where
    shannon :: Double -> Double
    shannon probX = probX * logBase (fromIntegral . natVal $ Proxy @base) (recip probX)

empiricalEntropyBase ::
  forall base t a.
  (KnownNat base, Foldable t, Ord a) =>
  t a ->
  Entropy base
empiricalEntropyBase = shannonEntropyBase . Bayes.uniformD . toList

higherOrderEntropyBase :: forall base t a. (KnownNat base, Foldable t, Ord a) => Natural -> t a -> Entropy base
higherOrderEntropyBase ctxSize seqOfSymbols =
  getSum $ SM.foldMapWithKey weighCtx (ctxOccsL seqOfSymbols ctxSize)
  where
    weighCtx :: [a] -> [a] -> Sum (Entropy base)
    weighCtx _ [] = Sum $ Entropy 0.0
    weighCtx [] _ = Sum $ Entropy 0.0
    weighCtx _ leftOcc =
      Sum $
        empiricalEntropyBase @base leftOcc
          * Entropy
            ( fromIntegral (length leftOcc)
                / fromIntegral (length seqOfSymbols)
            )

----------------------------------------------------------------------------------------------------

-- ** Helpers

-- | @O(n). `higherOrderEntropy` helper to collect of subsequences of length @n@ within the input
-- sequence.
--
--  @
--    >>> realSubsequencesN 2 ("abracadabra" :: String)
--    ["ab","br","ra","ac","ca","ad","da","ab","br","ra"]
--  @
--
--  As opposed to `subsequences`, which returns all possible sequences of the input string's symbols
-- rather than ones that actually exist in the input string:
--
--   @
--     >>> filter (\x -> length x == 2) $ subsequences ("abracadabra" :: String)
--     ["ab","ar","br","aa","ba","ra","ac","bc","rc","ac","aa","ba","ra","aa","ca","ad","bd","rd","ad","cd","ad","aa","ba","ra","aa","ca","aa","da","ab","bb","rb","ab","cb","ab","db","ab","ar","br","rr","ar","cr","ar","dr","ar","br","aa","ba","ra","aa","ca","aa","da","aa","ba","ra"]
--   @
realSubsequencesN :: Int -> [a] -> [[a]]
realSubsequencesN n s = allSubs' (length s) n s
  where
    allSubs' :: Int -> Int -> [a] -> [[a]]
    allSubs' l n' s'
      | l >= n' = take n' s' : allSubs' (l - 1) n' (fromJust $ tail `viaNonEmpty` s')
      | otherwise = []

-- | `higherOrderEntropy` helper to stratify characters with prefixes of n-length.
--
--  @
--    >>> ctxOccs ("abracadabra" :: String) 1
--    fromList [("a","bcdb"),("b","rr"),("c","a"),("d","a"),("r","aa")]
--    >>> ctxOccs ("abracadabra" :: String) 2
--    fromList [("ab","bb"),("ac","c"),("ad","d"),("br","rr"),("ca","a"),("da","a"),("ra","aa")]
--  @
ctxOccsL :: forall t a. (Foldable t, Ord a) => t a -> Natural -> Map [a] [a]
ctxOccsL = ctxOccs succ

-- | `higherOrderEntropy` helper to stratify characters with suffixes of n-length.
--
--  @
--    >>> ctxOccsR ("abracadabra" :: String) 1
--    fromList [("a","rcdr"),("b","a"),("c","a"),("d","a"),("r","bb")]
--  @
ctxOccsR :: forall t a. (Foldable t, Ord a) => t a -> Natural -> Map [a] [a]
ctxOccsR = ctxOccs pred

-- [@op@] looks at the neighbor index, which is either the prefix (`pred`) or suffix (`succ`)
ctxOccs :: forall t a. (Foldable t, Ord a) => (Int -> Int) -> t a -> Natural -> Map [a] [a]
ctxOccs op seqOfSymbols ctxSize = foldr insertObsCtx mempty allKOrderCtxs
  where
    allKOrderCtxs :: Set [a]
    allKOrderCtxs =
      foldr Set.insert mempty . realSubsequencesN (fromIntegral ctxSize) $ toList seqOfSymbols
    insertObsCtx :: [a] -> (Map [a] [a] -> Map [a] [a])
    insertObsCtx ctx =
      let matchIxs = KMP.match (KMP.build $ toList ctx) $ toList seqOfSymbols
          leftCtxIxs = filter (> 0) $ fmap op matchIxs
          leftOccs = mapMaybe (toList seqOfSymbols !!?) leftCtxIxs
       in insert (toList ctx) leftOccs

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
