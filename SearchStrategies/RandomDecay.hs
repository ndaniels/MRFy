{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module SearchStrategies.RandomDecay where

--import Data.List (last
import Control.Monad.Random
import qualified Data.Vector.Unboxed as V

import Beta
import LazySearchModel
--import MRFTypes
import Score
--import SearchStrategy
import StochasticSearch
import Viterbi

--import qualified SearchStrategies.RandomHillClimb as RHC

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


-- | This needs to be implemented properly based on counting triangles,
-- but for now, just a hack.
bogusPlacement :: RandomGen r => QuerySequence -> [BetaStrand] -> Rand r Placement
bogusPlacement _ [] = return []
bogusPlacement query (beta:betas) =
  place 0 (V.length query - sum (map len betas)) beta betas
  where place leftBound rightBound beta betas = do
          start <- getRandomR (leftBound, rightBound)
          rest  <- case betas of
                     [] -> return []
                     b : bs ->
                       place (leftBound + len beta) (rightBound + len b) b bs
          return $ start : rest
          

data Pop = Pop { improved :: Bool   -- ^ Better than previous generation
               , logPop   :: Double -- ^ log(ideal population)
               , logDecay :: Double -- ^ log(d), always negative
               , placements :: [Scored Placement] -- ^ true population
               }
-- ^ Invariant: the @placements@ list is *nearly* sorted
-- in order of decreasing score (best score at end).

scorePop :: Pop -> Scored Pop
scorePop (pop @ Pop { placements = ps }) = Scored pop (scoreOf $ minimum ps)

isort :: Ord a => [a] -> [a]
isort = foldr insert []
  where insert a [] = [a]
        insert a (b:bs)
          | a < b     = b : insert a bs
          | otherwise = insert a (b:bs)



firstPop :: Int -> Double -> Scorer Placement -> Rand r Placement -> Rand r Pop
firstPop n decay score placement = do
  ps <- fmap (map score) $ sequence $ take n $ repeat placement
  if decay < 1.0 && decay > 0.0 then
    return $ Pop True (log (fromIntegral n)) (log decay) ps
  else
    error "decay constant out of bounds"


nss :: NewSS
nss _hmm searchP query betas scorer = fullSearchStrategy
  (fmap scorePop $ firstPop nGens decay scorer (bogusPlacement query betas))
  undefined
  undefined
  undefined -- (takeNGenerations nGens)
  undefined -- (last . isort . placements)
  where popSize = getSearchParm searchP populationSize
        nGens   = generations searchP
        decay = exp (- log (fromIntegral popSize) / fromIntegral nGens)


mutate :: forall r
       .  RandomGen r
       => SearchParameters
       -> QuerySequence
       -> [BetaStrand]
       -> Scorer Placement
       -> Scored Placement
       -> Rand r (Scored Placement)
mutate _ query betas scorer (Scored oldp _) =
  fmap scorer $ mutate oldp 0 0
  where mutate :: Placement -> Int -> Int -> Rand r Placement
        mutate [] _ _ = return []
        mutate _p@(_:gs) i lastGuess = do -- loop inv: i+length _p == length oldp
          g'  <- getRandomR $ betaRange query betas oldp lastGuess i
          gs' <- mutate gs (i+1) g'
          return $ g' : gs'

        _move i leftBound =
          if i == length oldp then return []
          else
            do g  <- getRandomR (leftBound, rightBound oldp i query - width)
               gs <- _move (i+1) (g + width)
               return $ g : gs
          where width = len (betas !! i)
              
        betaRange = undefined
        rightBound = undefined
        
