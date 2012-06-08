{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
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
  (fmap scorer $ initialize hmm searchP query betas)
  (mutate searchP query betas scorer)
  scoreUtility
  (takeByAgeGap (acceptableAgeGap searchP))
  id

initialize :: HMM -> SearchParameters -> QuerySequence
           -> [BetaStrand] -> Rand r Placement
initialize hmm searchP query betas =
  projInitialGuess hmm (getSecPreds searchP) query betas

-- invariant: len [SearchSolution] == 1
mutate :: forall r
       .  RandomGen r
       => SearchParameters
       -> QuerySequence
       -> [BetaStrand]
       -> Scorer Placement
       -> Scored Placement
       -> Rand r (Scored Placement)
mutate searchP query betas scorer (Scored oldp _) =
  fmap scorer $ mutate oldp 0 0
  where mutate :: Placement -> Int -> Int -> Rand r Placement
        mutate [] _ _ = return []
        mutate (g:gs) i lastGuess = do
          g'  <- getRandomR $ betaRange query betas oldp lastGuess i
          gs' <- mutate gs (i+1) g'
          return $ g' : gs'
       
betaRange :: QuerySequence -> [BetaStrand] -> Placement -> Int -> Int -> (Int, Int)
betaRange query betas oldp lastGuess i = (leftBound, rightBound - len (betas !! i))
  where leftBound  = if i == 0 then 0
                     else len (betas !! pred i) + lastGuess
        rightBound = if succ i < length oldp then
                       oldp !! succ i
                     else
                       V.length query  
