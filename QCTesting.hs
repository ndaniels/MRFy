module QCTesting where

import qualified Data.Vector as V
import Test.QuickCheck

import Beta
import HmmPlus
import qualified SearchStrategies.GeneticAlgorithm as GA
import StochasticSearch

verifySearchGuess :: HMM -> [BetaStrand] -> SearchGuess -> Bool
verifySearchGuess _ [] [] = True
verifySearchGuess _ [] [g] = False
verifySearchGuess _ [b] [] = False
verifySearchGuess hmm (b:bs) (g:gs) = range && noClash
  where range = g >= 0 && (g + len b) <= V.length hmm
        noClash = case gs of
                    [] -> True
                    (g':gs') -> g' > g + len b

