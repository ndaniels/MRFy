{-# LANGUAGE BangPatterns #-}
module SearchStrategies.RandomHillClimb where

import qualified Data.Vector as V
import System.Random (mkStdGen, randomR, StdGen)

import Debug.Trace (trace)

import Beta
import Constants
import HMMPlus
import MRFTypes
import Score
import SearchModel
import SearchStrategy
import StochasticSearch
import Viterbi

nss :: NewSS
nss hmm searchP query betas =
      SS { gen0 = \seed -> initialize' hmm searchP seed query betas 
         , nextGen = \seed scorer placements ->
                      mutate' searchP query betas seed scorer placements
         , accept = \seed hist age ->
                     accept' searchP seed hist age
         , quit = \hist age ->
                   terminate' searchP hist age
         }
initialize' :: HMM -> SearchParameters -> Seed -> QuerySequence -> [BetaStrand] -> [Placement]
initialize' hmm searchP seed query betas = [projInitialGuess hmm (getSecPreds searchP) seed query betas]

accept' :: SearchParameters -> Seed -> History Placement -> Age -> Bool
accept' _ _ (History ps) _ = ok ps
  where ok []        = error "empty history passed to accept predicate" 
        ok [s1]      = True 
        ok ((s1,_):(s2,_):_) = scoreOf s1 < scoreOf s2 
-- XXX TODO this code is a duplicate of similar code in GeneticAlgorithm.hs

terminate' :: SearchParameters -> History placement -> Age -> Bool
terminate' searchP (!hist) age = (showMe $ not $ age < (generations searchP))
                                    || converge searchP hist age
  where showMe = if not $ (10.0 * ((fromIntegral age)
                                   / (fromIntegral (generations searchP))))
                          `elem` [1.0..10.0] then
                   id
                 else
                   trace ((show age) ++ " generations complete")

converge :: SearchParameters -> History placement -> Age -> Bool
converge searchP (History ((_,a):as)) age = 
    case maxGap of
        Just x -> a < age - x
        Nothing -> False
  where maxGap = convergenceAge searchP

-- invariant: len [SearchSolution] == 1
mutate' :: SearchParameters
        -> QuerySequence
        -> [BetaStrand]
        -> Seed
        -> Scorer Placement
        -> [Scored Placement]
        -> [Scored Placement]
mutate' searchP query betas seed scorer placements =
  [scorer $ mutate'' oldp 0 (mkStdGen seed) 0]
  where oldp = unScored $ head placements

        mutate'' :: Placement -> Int -> StdGen -> Int -> Placement
        mutate'' [] _ _ _ = []
        mutate'' (g:gs) i gen lastGuess = g' : mutate'' gs (i+1) gen' g'
          where (g', gen') = randomR (lo, hi) gen
                lo = if i == 0 then
                       0
                     else
                       (len $ (betas !! (i - 1))) + lastGuess
                hi = if i == (length oldp) - 1 then
                       V.length query - (len $ betas !! i)
                     else
                       (oldp !! (i + 1)) - (len $ betas !! i)

