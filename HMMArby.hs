{-# LANGUAGE FlexibleInstances #-}
module HMMArby where

import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random
import Test.QuickCheck
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


instance Arbitrary (V.Vector Int) where
  arbitrary = fmap V.fromList arbitrary

-- Generating query sequences must generate lists of integers
-- in the range [0, number of amino acids in alphabet).
instance Arbitrary (U.Vector Int) where
  arbitrary = do
    let aas = choose (0, pred $ length aminoList) :: Gen Int
    seq <- listOf aas :: Gen [Int]
    return $ U.fromList seq

prob :: Gen Double
prob = choose (0.0, 1.0)

instance Arbitrary (V.Vector HMMNode) where
  arbitrary = do
    -- A random length is used here to essentially guarantee
    -- that we get a list of HMMNodes with at least length 2.
    -- Is there a better way to do this? (Preferably without
    -- setting a maximum.)
    randLen <- choose (2, 100)
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
    rTrans <- arbitrary :: Gen TransitionProbabilities
    return HMMNode { nodeNum = 0
                   , matchEmissions     = toScoreVec rMatEmi
                   , insertionEmissions = toScoreVec rInsEmi
                   , transitions = rTrans
                   }
    where toScoreVec = U.fromList . map toScore

instance Arbitrary TransitionProbabilities where
  arbitrary = do
    ((p1, p2, p3, p4), (p5, p6, p7, p8, p9)) <- arbitrary   
    -- The from/to states in TransitionProbability must correspond
    -- to their place in TransitionProbabilities.
    return $ TransitionProbabilities
      p1 { fromState = Mat, toState = Mat}
      p2 { fromState = Mat, toState = Ins}
      p3 { fromState = Mat, toState = Del}
      p4 { fromState = Ins, toState = Mat}
      p5 { fromState = Ins, toState = Ins}
      p6 { fromState = Del, toState = Mat}
      p7 { fromState = Del, toState = Del}
      p8 { fromState = Beg, toState = Mat}
      p9 { fromState = Mat, toState = End}

-- Fix this so that only legal transitions are allowed.
instance Arbitrary TransitionProbability where
  arbitrary = do
    rLogProb <- arbitrary :: Gen Score
    -- fromState and toState should *not* be randomly generated
    -- here. Their values can only be known by the
    -- TransitionProbabilities generator.
    return $ TransitionProbability { logProbability = rLogProb
                                   , fromState = Mat
                                   , toState = Mat
                                   }

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
