module SearchStrategy where

import qualified Data.Vector as V
import System.Random (mkStdGen, random, randomR, StdGen)

import Beta
import NonUniform
import StochasticSearch
import Viterbi

initialGuess :: Seed -> QuerySequence -> [BetaStrand] -> SearchGuess
initialGuess seed qs betas = initialGuess' betas 0 $ mkStdGen seed
  where initialGuess' :: [BetaStrand] -> Int -> StdGen -> SearchGuess
        initialGuess' [] _ _ = []
        initialGuess' (b:bs) lastGuess gen = pos : initialGuess' bs (pos + len b) gen'
          where (pos, gen') = randomR (lastGuess, betaSum) gen
                betaSum = V.length qs - (foldr (+) 0 $ map len (b:bs))

geoInitialGuess :: Seed -> QuerySequence -> [BetaStrand] -> SearchGuess
geoInitialGuess seed qs betas = initialGuess' betas 0 $ mkStdGen seed
  where initialGuess' :: [BetaStrand] -> Int -> StdGen -> SearchGuess
        initialGuess' [] _ _ = []
        initialGuess' (b:bs) lastGuess gen = pos : initialGuess' bs (pos + len b) gen'
          where pos = head rand
                betaSum = V.length qs - (foldr (+) 0 $ map len (b:bs))
                (seed, gen') = random gen

                rand = (randomsDist (mkStdGen seed) 
                        $ zip [lastGuess..] 
                        $ map ((/) 1.0) 
                        $ take (betaSum - lastGuess)
                        $ map (** 1) ([1.0, 2.0..] :: [Double])) :: [Int]

