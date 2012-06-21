{-# LANGUAGE BangPatterns #-}
module SearchStrategies.GeneticAlgorithm where

import Control.Parallel (par)
import Control.Parallel.Strategies

import Data.List
import qualified Data.Vector.Unboxed as V
import System.Random (mkStdGen, randomR, randoms, StdGen)

import Debug.Trace (trace)

import Beta
import Constants
import HMMPlus
import MRFTypes
import NonUniform
import Score
import SearchModel
import SearchStrategy
import Shuffle
import StochasticSearch
import Viterbi

nss :: NewSS
nss hmm searchP query betas =
  SS { gen0 = \seed -> initialize' hmm searchP seed query betas 
     , nextGen = mutate' searchP query betas
     , accept = histProgresses scoreProgresses
     , quit = terminate' searchP 
     }

initialize' :: HMM -> SearchParameters -> Seed -> QuerySequence -> [BetaStrand] -> [Placement]
initialize' hmm searchP seed query betas = 
  map (\s -> projInitialGuess hmm (getSecPreds searchP) s query betas)
      $ take (getSearchParm searchP populationSize) rands
  where rands = (randoms (mkStdGen seed)) :: [Int]

terminate' :: SearchParameters -> History a -> Age -> Bool
terminate' searchP (!_scores) age = showMe $ not $ age < (generations searchP)
  where showMe = if not $ (10.0 * ((fromIntegral age)
                                   / (fromIntegral (generations searchP))))
                          `elem` [1.0..10.0] then
                   id
                 else
                   trace ((show age) ++ " generations complete")

-- invariant: len [SearchSolution] == 1
mutate' :: SearchParameters
        -> QuerySequence
        -> [BetaStrand]
        -> Seed
        -> Scorer Placement
        -> [Scored Placement]
        -> [Scored Placement]
mutate' searchP query betas seed scorer placements = fittest
  where fittest = fst
                  $ shuffle (mkStdGen seed)
                  $ take (getSearchParm searchP populationSize)
                  $ sort
                  $ placements ++ progeny
        progeny = (parMap rseq) scorer
                  $ map (\gs -> mutateChild 0 0 (mkStdGen seed) gs gs)
                  $ getPairings
                  $ map unScored placements

        mutateChild :: Int -> Int -> StdGen -> Placement -> Placement -> Placement
        mutateChild _ _ _ _ [] = []
        mutateChild i lastGuess gen ogs (g:gs) = g' : mutateChild (i+1) g' gen' ogs gs
          where (g', gen') = randomR (lo, hi) gen
                lo = if i == 0 then
                       0
                     else
                       (len $ (betas !! (i - 1))) + lastGuess
                hi = if i == (length ogs) - 1 then
                       V.length query - (len $ betas !! i)
                     else
                       (ogs !! (i + 1)) - (len $ betas !! i)

getPairings :: [Placement] -> [Placement]
getPairings [] = []
getPairings [p] = [p]
getPairings (p1:p2:ps) = crossover p1 p2 : getPairings ps

-- mutateChild :: StdGen -> SearchGuess -> SearchGuess 
-- mutateChild = mutateChild' 0 0 

crossover :: Placement -> Placement -> Placement
crossover ps qs = sort $ crossover' ps qs

-- invariant: length ps == length qs
crossover' :: Placement -> Placement -> Placement
crossover' [] [] = []
crossover' [p] [q] = if p < q then [p] else [q]
crossover' (p:ps) (q:qs) = leftmost:rightmost:crossover' (init ps) (init qs)
  where leftmost = if p < q then p else q
        rightmost = if lastp > lastq then lastp else lastq
        lastp = last ps
        lastq = last qs

