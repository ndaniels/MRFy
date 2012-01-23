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
        initialGuess' (b:bs) lastGuess gen = pos : initialGuess' bs (pos + len b) gen'
          where (pos, gen') = randomR (lastGuess, betaSum) gen
                betaSum = V.length qs - (foldr (+) 0 $ map len (b:bs))

        

