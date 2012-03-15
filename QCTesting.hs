{-# LANGUAGE FlexibleInstances #-}
module QCTesting where

import qualified Data.Vector as V
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
-- import Data.List 

import Beta
import Constants
import HmmPlus
import qualified SearchStrategies.GeneticAlgorithm as GA
import StochasticSearch
import Viterbi

import Debug.Trace (trace)

verifySearchGuess :: HMM -> [BetaStrand] -> SearchGuess -> Bool
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

instance Arbitrary HmmNode where
  arbitrary = do
    rMatEmi <- vectorOf 20 $ choose (0.0, 1.0) :: Gen [Double]
    rInsEmi <- vectorOf 20 $ choose (0.0, 1.0) :: Gen [Double]
    rTrans <- arbitrary :: Gen TransitionProbabilities
    return HmmNode { nodeNum = 0
                   , matchEmissions = toLogVec rMatEmi
                   , annotations = Nothing
                   , insertionEmissions = toLogVec rInsEmi
                   , transitions = rTrans
                   }
    where toLogVec = V.fromList . map toLogProb

instance Arbitrary TransitionProbabilities where
  arbitrary = do
    ((p1, p2, p3, p4), (p5, p6, p7, p8, p9)) <- arbitrary   
    return $ TransitionProbabilities p1 p2 p3 p4 p5 p6 p7 p8 p9

instance Arbitrary TransitionProbability where
  arbitrary = do
    rLogProb <- arbitrary :: Gen LogProbability
    from <- elements [Mat, Ins, Del] :: Gen HMMState
    to <- elements [Mat, Ins, Del] :: Gen HMMState
    return $ TransitionProbability { logProbability = rLogProb
                                   , fromState = from
                                   , toState = to
                                   }

instance Arbitrary LogProbability where
  arbitrary = do
    f <- choose (0.0, 1.0) :: Gen Double
    if f == 0.0 then
      do return LogZero
    else do return $ HmmPlus.NonZero $ toLogProb f

instance Arbitrary HMMState where
  -- Test this with sample (arbitrary :: Gen HMMState)
  arbitrary = elements [Mat, Ins, Del, Beg, End, BMat] 

toLogProb :: Double -> Double
toLogProb f = if f == 0.0 then maxProb else (-(log f))
 


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