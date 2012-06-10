{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module SearchStrategies.RandomDecay where

import Control.Monad.Random
import Data.List (sortBy)
import qualified Data.Vector.Unboxed as V

import LazySearchModel
import ParRandom
import Score
import SearchStrategy
import StochasticSearch

import qualified SearchStrategies.RandomHillClimb as RHC

{-

Overview: Start with a population of placements that is uniformly
distributed over the space of placements.  At each generation, the
size of the population decays exponentially by the equation 

  |P'| = d * |P|

Also at each generation, each individual member of the population is
improved if possilbe.  The population as a whole improves if any
individual improves.

The exponential decay guarantees a total linear amount of work in the
number of generations and initial population size, but it might be
useful also to cull based on differences in score or on other
criteria.

-}


-- | Represents a decaying population of placements
data Pop = Pop { improved :: Bool   -- ^ Better than previous generation
               , logPop   :: Double -- ^ log(ideal population)
               , placements :: [Scored Placement] -- ^ true population
               }
-- ^ Invariant: the @placements@ list is *nearly* sorted
-- in order of decreasing score (best score at end).
-- Invariant: @length placements == ceiling (exp logPop)@.


firstPop :: Int -> Scorer Placement -> Rand r Placement -> Rand r Pop
firstPop n score placement = do
  ps <- fmap (map score) $ sequence $ take n $ repeat placement
  return $ Pop True (log (fromIntegral n)) (sortBy (flip compare) ps)

nss :: NewSS
nss _hmm searchP query betas scorer = fullSearchStrategy
  (fmap scorePop $ firstPop nGens scorer (equalPlacement query betas))
  (fmap scorePop . nextPop nextPlacement scorer logDecay . unScored)
  scoreUtility -- bogus!  see comment on scorePop below
  (takeNGenerations nGens)
  (unScored . last . isort . placements)
  where popSize = getSearchParm searchP populationSize
        nGens   = generations searchP
        logDecay = - log (fromIntegral popSize) / fromIntegral nGens
        nextPlacement = flip (RHC.randomizePlacement' betas) (V.length query)


nextPop :: RandomGen r => (Placement -> Rand r Placement) -> Scorer Placement -> Double
        -> (Pop -> Rand r Pop)
nextPop nextPlacement scorer logDecay pop =
  do randomSteps <- parRandom $
                    map (fmap scorer . nextPlacement . unScored) oldPlacements
     let newPlacements = zipWith min randomSteps oldPlacements
         hasBetter     = or $ zipWith (<) newPlacements oldPlacements
     return $ Pop hasBetter logPop' (cull newPlacements)
  where oldPlacements = placements pop
        logPop' = logPop pop + logDecay
        cull ps = if logPop' >= 0.0 && ceiling logPop' < length ps then
                    drop (length ps - ceiling logPop') (isort ps)
                  else
                    ps


        
-- | A population's score is the average score if its members.
-- This way if *any* score improves, the whole population improves,
-- because when we make the new population, we keep only
-- those scores that have improved.
scorePop :: Pop -> Scored Pop
scorePop (pop @ Pop { placements = ps }) = Scored pop (Score average)
  where average = (unScore . sum . map scoreOf) ps  / (fromIntegral . length) ps

-- | Insertion sort.  We use it because we expect the placements list
-- to be sorted or nearly sorted, so that the expected cost is around
-- linear.
        
isort :: Ord a => [a] -> [a]
isort = foldr insert []
  where insert a [] = [a]
        insert a (b:bs)
          | a < b     = b : insert a bs
          | otherwise = insert a (b:bs)
