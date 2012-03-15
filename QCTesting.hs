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
    transProbs <- vectorOf 9 arbitrary :: Gen [TransitionProbability]
    return $ TransitionProbabilities { m_m = transProbs !! 0
                                     , m_i = transProbs !! 1
                                     , m_d = transProbs !! 2
                                     , i_m = transProbs !! 3
                                     , i_i = transProbs !! 4
                                     , d_m = transProbs !! 5
                                     , d_d = transProbs !! 6
                                     , b_m = transProbs !! 7
                                     , m_e = transProbs !! 8
                                     }

instance Arbitrary TransitionProbability where
  arbitrary = do
    rLogProb <- arbitrary :: Gen LogProbability
    rstate1 <- elements [Mat, Ins, Del] :: Gen HMMState
    rstate2 <- elements [Mat, Ins, Del] :: Gen HMMState
    return $ TransitionProbability { logProbability = rLogProb
                                   , fromState = rstate1
                                   , toState = rstate2
                                   }

instance Arbitrary LogProbability where
  arbitrary = do
    f <- choose (0.0, 1.0) :: Gen Double
    if f == 0.0 then
      do return LogZero
    else do return $ HmmPlus.NonZero $ toLogProb f

instance Arbitrary HMMState where
  -- This is not working... It returns 'BMat' every time
  -- arbitrary = elements [Mat, Ins, Del, Beg, End, BMat] 
  arbitrary = do
    n <- choose (0, 5) :: Gen Int
    return $ trace ("##" ++ show n ++ "##") $ case n of
                  0 -> Mat
                  1 -> Ins
                  2 -> Del
                  3 -> Beg
                  4 -> End
                  5 -> BMat

toLogProb :: Double -> Double
toLogProb f = if f == 0.0 then maxProb else (-(log f))
 

genQuery :: Int -> Int -> [QuerySequence]
genQuery seed upper = unGen arbitrary (mkStdGen seed) upper

genNodes :: Int -> Int -> HmmNode
genNodes seed upper = (unGen arbitrary) (mkStdGen seed) upper

genState :: Int -> HMMState
genState seed = (unGen arbitrary) (mkStdGen seed) 1000

