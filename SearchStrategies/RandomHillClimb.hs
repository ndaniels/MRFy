{-# LANGUAGE BangPatterns #-}
module SearchStrategies.RandomHillClimb where

import Control.Monad.Random
import qualified Data.Vector.Unboxed as V
import System.Random (mkStdGen, randomR, StdGen, next)

import Debug.Trace (trace)

import Beta
import Constants
import HMMPlus
import LazySearchModel
import MRFTypes
import Score
import SearchStrategy
import StochasticSearch
import Viterbi

nss :: NewSS
nss hmm searchP query betas scorer = fullSearchStrategy
  (\seed -> scorer $ initialize hmm searchP seed query betas)
  (mutate searchP query betas scorer)
  scoreUtility
  (takeByAgeGap (acceptableAgeGap searchP))
  id

initialize :: HMM -> SearchParameters -> Seed -> QuerySequence
           -> [BetaStrand] -> Placement
initialize hmm searchP seed query betas =
  projInitialGuess hmm (getSecPreds searchP) seed query betas

-- invariant: len [SearchSolution] == 1
mutate :: SearchParameters
        -> QuerySequence
        -> [BetaStrand]
        -> Scorer Placement
        -> Seed
        -> Scored Placement
        -> Scored Placement
mutate searchP query betas scorer seed (Scored oldp _) =
  scorer $ mutate oldp 0 (mkStdGen seed) 0
  where mutate :: Placement -> Int -> StdGen -> Int -> Placement
        mutate [] _ _ _ = []
        mutate (g:gs) i gen lastGuess = g' : mutate gs (i+1) gen' g'
          where (g', gen') = randomR range gen
                range = betaRange query betas oldp lastGuess i

       
betaRange :: QuerySequence -> [BetaStrand] -> Placement -> Int -> Int -> (Int, Int)
betaRange query betas oldp lastGuess i = (leftBound, rightBound - len (betas !! i))
  where leftBound  = if i == 0 then 0
                     else len (betas !! pred i) + lastGuess
        rightBound = if succ i < length oldp then
                       oldp !! succ i
                     else
                       V.length query  

singleton :: a -> [a]
singleton a = [a]
