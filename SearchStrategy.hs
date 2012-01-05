module SearchStrategy where

import qualified Data.Vector as V
import System.Random (mkStdGen, randomR, StdGen)

import Beta
import StochasticSearch
import Viterbi

initialGuess :: Seed -> QuerySequence -> [BetaStrand] -> SearchGuess
initialGuess seed qs betas = initialGuess' betas 0 $ mkStdGen seed
  where initialGuess' :: [BetaStrand] -> Int -> StdGen -> SearchGuess
        initialGuess' [] _ _ = []
        initialGuess' (b:bs) start gen = pos : initialGuess' bs (start + betaLen) gen'
          where (pos, gen') = randomR (start, betaSum) gen
                betaLen = len b
                betaSum = V.length qs - (foldr (+) 0 $ map len bs)

        

