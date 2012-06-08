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
import SearchStrategy 
import SearchStrategies.RandomHillClimb (betaRange)
import LazySearchModel
import Shuffle
import StochasticSearch
import Viterbi

wrapBestScore :: [Scored a] -> Scored [Scored a]
wrapBestScore as = Scored as (scoreOf $ minimum as)


nss :: NewSS
nss hmm searchP query betas scorer = fullSearchStrategy
  (\seed -> wrapBestScore $ map scorer $ initialize hmm searchP seed query betas)
  (mutate searchP query betas scorer)
  scoreUtility
  (takeNGenerations (generations searchP))
  (unScored . minimum)

type Population = [Scored Placement]

initialize
  :: HMM -> SearchParameters -> Seed -> QuerySequence -> [BetaStrand] -> [Placement]
initialize hmm searchP seed query betas = 
  map (\s -> projInitialGuess hmm (getSecPreds searchP) s query betas)
      $ take (getSearchParm searchP populationSize) rands
  where rands = randoms (mkStdGen seed) :: [Int]

-- invariant: len [SearchSolution] == 1
mutate :: SearchParameters
        -> QuerySequence
        -> [BetaStrand]
        -> Scorer Placement
        -> Seed
        -> Scored Population
        -> Scored Population
mutate searchP query betas scorer seed (Scored placements _) = wrapBestScore fittest
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
          where (g', gen') = randomR range gen
                range = betaRange query betas ogs lastGuess i

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

