{-# LANGUAGE FlexibleInstances #-}
module QCTesting where

import qualified Data.Vector as V
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
  arbitrary = do
    let aas = choose (0, 20) :: Gen Int
    seq <- listOf aas :: Gen [Int]
    return $ V.fromList seq

instance Arbitrary HMMNode where
  arbitrary = do
    rMatEmi <- vectorOf 20 $ choose (0.0, 1.0) :: Gen [Double]
    rInsEmi <- vectorOf 20 $ choose (0.0, 1.0) :: Gen [Double]
    rTrans <- arbitrary :: Gen TransitionProbabilities
    return HMMNode { nodeNum = 0
                   , matchEmissions = toScoreVec rMatEmi
                   , insertionEmissions = toScoreVec rInsEmi
                   , transitions = rTrans
                   }
    where toScoreVec = V.fromList . map toScore

instance Arbitrary TransitionProbabilities where
  arbitrary = do
    ((p1, p2, p3, p4), (p5, p6, p7, p8, p9)) <- arbitrary   
    return $ TransitionProbabilities p1 p2 p3 p4 p5 p6 p7 p8 p9

instance Arbitrary TransitionProbability where
  arbitrary = do
    rLogProb <- arbitrary :: Gen Score
    from <- elements [Mat, Ins, Del] :: Gen HMMState
    to <- elements [Mat, Ins, Del] :: Gen HMMState
    return $ TransitionProbability { logProbability = rLogProb
                                   , fromState = from
                                   , toState = to
                                   }

instance Arbitrary Score where
  arbitrary = do
    f <- choose (0.0, 1.0) :: Gen Double
    return $ if f == 0.0 then negLogZero
             else Score $ - log f

instance Arbitrary HMMState where
  -- Test this with sample (arbitrary :: Gen HMMState)
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