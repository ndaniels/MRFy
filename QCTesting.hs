{-# LANGUAGE FlexibleInstances #-}
module QCTesting where

import qualified Data.Vector as V
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
-- import Data.List 

import Beta
import HmmPlus
import qualified SearchStrategies.GeneticAlgorithm as GA
import StochasticSearch
import Viterbi

instance Arbitrary (V.Vector Int) where
  arbitrary = do
    let aas = choose(0, 20) :: Gen Int
    seq <- listOf aas :: Gen [Int]
    return $ V.fromList seq
 

verifySearchGuess :: HMM -> [BetaStrand] -> SearchGuess -> Bool
verifySearchGuess _ [] [] = True
verifySearchGuess _ [] [g] = False
verifySearchGuess _ [b] [] = False
verifySearchGuess hmm (b:bs) (g:gs) = range && noClash
  where range = g >= 0 && (g + len b) <= V.length hmm
        noClash = case gs of
                    [] -> True
                    (g':gs') -> g' > g + len b

genQuery :: Int -> [QuerySequence]
genQuery n = unGen arbitrary (mkStdGen 10) n

