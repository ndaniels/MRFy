{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module HMMArby where

import Control.Applicative
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
-- import Data.List 

import Beta
import Constants
import HMMPlus
import MRFTypes
import qualified SearchStrategies.GeneticAlgorithm as GA
import Score
import StochasticSearch
import Viterbi

import Debug.Trace (trace)

verifySearchGuess :: HMM -> [BetaStrand] -> Placement -> Bool
verifySearchGuess _ [] [] = True
verifySearchGuess _ [] [g] = False
verifySearchGuess _ [b] [] = False
verifySearchGuess hmm (b:bs) (g:gs) = range && noClash
  where range = g >= 0 && (g + len b) <= V.length hmm
        noClash = case gs of
                    [] -> True
                    (g':gs') -> g' > g + len b

instance Arbitrary Query
  

instance Arbitrary QuerySequence where
  shrink = (map U.fromList) . (shrink . U.toList)
  arbitrary = do
    qs <- arbitrary
    return $ U.fromList $ take (min (length qs) 12) qs

instance Arbitrary AA where
  arbitrary = arbitraryBoundedIntegral

prob :: Gen Double
prob = choose (0.0, 1.0)

instance Arbitrary (V.Vector HMMNode) where
  shrink = (map V.fromList) . filter (\xs -> length xs >= 2) . (shrink . V.toList)
  arbitrary = do
    -- A random length is used here to essentially guarantee
    -- that we get a list of HMMNodes with at least length 2.
    -- Is there a better way to do this? (Preferably without
    -- setting a maximum.)
    randLen <- choose (2, 12)
    nodes <- vectorOf randLen arbitrary :: Gen [HMMNode]
    return $ V.fromList $ map annotate $ zip [0..] nodes

    -- Add incrementing node numbers to [HMMNode].
    where annotate (i, node) = node { nodeNum = i }

-- Make node numbers valid. I think that should be
-- done in `Arbitrary (V.Vector HMMNode)`.
instance Arbitrary HMMNode where
  arbitrary = do
    -- (rMatEmi, rInsEmi) <- fmap unzip $ listOf1 (liftM2 (,) prob prob) 
    rMatEmi <- sequence $ take (length aminoList) $ repeat prob
    rInsEmi <- sequence $ take (length aminoList) $ repeat prob
    rTrans <- arbitrary :: Gen TProbs
    return HMMNode { nodeNum = 0
                   , matEmissions = toScoreVec rMatEmi
                   , insEmissions = toScoreVec rInsEmi
                   , transitions  = rTrans
                   }
    where toScoreVec = U.fromList . map toScore

instance Arbitrary TProbs where
  arbitrary = mkTransProbs <$> p <*> p <*> p <*> p <*> p <*> p <*> p
   where p = arbitrary   

-- Fix this so that only legal transitions are allowed.
instance Arbitrary TProb where
  arbitrary = TProb <$> arbitrary

instance Arbitrary Score where
  arbitrary = do
    f <- choose (0.0, 1.0) :: Gen Double
    return $ if f == 0.0 then negLogZero
             else Score $ - log f

instance Arbitrary StateLabel where
  -- Test this with sample (arbitrary :: Gen StateLabel)
  arbitrary = elements [Mat, Ins, Del, Beg, End, BMat] 

-- | Map a probability to a score
toScore :: Double -> Score
toScore f = if f == 0.0 then negLogZero else Score (- log f)
 


-- possibly useful idea

class Probabilities a where
    normalize :: a -> a  -- normalize a group of probabilities

instance Probabilities (Double, Double, Double) where
    normalize (x, y, z) = if sum == 0.0 then (x, y, z)
                          else (x / sum, y / sum, z / sum)
      where sum = x + y + z

instance Probabilities (Double, Double) where
    normalize (x, y) = if sum == 0.0 then (x, y)
                          else (x / sum, y / sum)
      where sum = x + y

instance Probabilities (V.Vector Double) where
  normalize xs = if sum == 0.0 then xs
                 else V.map (/ sum) xs
    where sum = V.foldl' (+) 0.0 xs
