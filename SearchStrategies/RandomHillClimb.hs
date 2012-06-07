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
nss hmm searchP query betas scorer = searchStrategy
  (\seed -> map scorer $ initialize hmm searchP seed query betas)
  (mutate searchP query betas scorer)
  scoreUtility
  (takeByAgeGap (acceptableAgeGap searchP))

initialize :: HMM -> SearchParameters -> Seed -> QuerySequence
           -> [BetaStrand] -> [Placement]
initialize hmm searchP seed query betas =
  [projInitialGuess hmm (getSecPreds searchP) seed query betas]

-- invariant: len [SearchSolution] == 1
mutate :: SearchParameters
        -> QuerySequence
        -> [BetaStrand]
        -> Scorer Placement
        -> Seed
        -> [Scored Placement]
        -> [Scored Placement]
mutate searchP query betas scorer seed placements =
  [scorer $ mutate' oldp 0 (mkStdGen seed) 0]
  where oldp = unScored $ head placements

        mutate' :: Placement -> Int -> StdGen -> Int -> Placement
        mutate' [] _ _ _ = []
        mutate' (g:gs) i gen lastGuess = g' : mutate' gs (i+1) gen' g'
          where (g', gen') = randomR range gen
                range = betaRange query betas oldp lastGuess i

betaRange :: QuerySequence -> [BetaStrand] -> Placement -> Int -> Int -> (Int, Int)
betaRange query betas oldp lastGuess i = (lo, hi)
  where lo = if i == 0 then
               0
             else
               len (betas !! pred i) + lastGuess
        hi = if succ i < length oldp then
               (oldp !! succ i) - len (betas !! i)
             else
               V.length query   - len (betas !! i)



-- | Alternative implementation.  Compose me with (singleton . scorer)
randomPlacements :: HMM
                 -> SearchParameters
                 -> QuerySequence
                 -> [BetaStrand]
                 -> StdGen
                 -> [Placement]
randomPlacements hmm searchP query betas gen =
  map fst $ iterate step (first, gen')
  where first = projInitialGuess hmm (getSecPreds searchP) seed query betas
        (seed, gen') = next gen
        step (oldp, gen) = runRand (mutate oldp 0 0) gen
          where mutate :: Placement -> Int -> Int -> Rand StdGen Placement
                mutate [] _ _ = return []
                mutate (g:gs) i lastGuess =
                  do g'  <- getRandomR $ betaRange query betas oldp lastGuess i
                     gs' <- mutate gs (succ i) g'
                     return $ g' : gs'

  