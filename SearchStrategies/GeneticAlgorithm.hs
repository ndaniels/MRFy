module SearchStrategies.GeneticAlgorithm where

import Control.Monad.Random
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
  (fmap (wrapBestScore . map scorer) $ initialize hmm searchP query betas)
  (mutate searchP query betas scorer)
  scoreUtility
  (takeNGenerations (generations searchP))
  (unScored . minimum)

type Population = [Scored Placement]

initialize
  :: RandomGen r
  => HMM -> SearchParameters -> QuerySequence -> [BetaStrand] -> Rand r [Placement]
initialize hmm searchP query betas = 
  sequence $ take n $ repeat $ projInitialGuess hmm (getSecPreds searchP) query betas
  where n = getSearchParm searchP populationSize

-- invariant: len [SearchSolution] == 1
mutate :: SearchParameters
       -> QuerySequence
       -> [BetaStrand]
       -> Scorer Placement
       -> Scored Population
       -> Rand StdGen (Scored Population)
mutate searchP query betas scorer (Scored placements _) = fmap wrapBestScore fittest
  where fittest = do gen <- getSplit
                     return
                       $ fst
                       $ shuffle gen
                       $ take (getSearchParm searchP populationSize)
                       $ sort
                       $ placements ++ progeny gen
        progeny gen = (parMap rseq) scorer
                      $ map (\gs -> mutateChild 0 0 gen gs gs)
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

