{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module SearchStrategies.RandomHillClimb where

import Control.Monad.LazyRandom
import qualified Data.Vector.Unboxed as V

import Beta
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

initialize :: RandomGen r => HMM -> SearchParameters -> QuerySequence
           -> [BetaStrand] -> Rand r Placement
initialize hmm searchP query betas =
  projInitialGuess hmm (getSecPreds searchP) query betas

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
              

-- | Calling @randomizePlacement betas oldp n@ returns a new placement in which
-- each beta strand is placed in a random location chosen uniformly between
-- its left and right bounds.  The left and right bound of each beta
-- strand is the adjacent beta strand; where there is no adjacent beta
-- strand, the bound is 0 on the left and @n@ on the right.  The representation
-- is a list @betas@ of beta strands and a list @oldp@ of their leftmost positions.
--
-- precondition: length betas == length oldp && maxRight >= sum betas
randomizePlacement 
  :: RandomGen r => [BetaStrand] -> Placement -> Int -> Rand r Placement
randomizePlacement betas oldp maxRight = shiftFrom 0 0
  where shiftFrom i leftBound =
          if i == length oldp then return []
          else do g  <- getRandomR (leftBound, rightBound - width)
                  gs <- shiftFrom (i+1) (g+width)
                  return $ g : gs
            where width = len (betas !! i)
                  rightBound = if succ i < length oldp then oldp !! succ i
                               else maxRight

-- here's another version that passes a list of right bounds
randomizePlacement' 
  :: RandomGen r => [BetaStrand] -> Placement -> Int -> Rand r Placement
randomizePlacement' betas oldp maxRight = shiftFrom betas 0 (tail oldp ++ [maxRight])
  where shiftFrom (beta:betas) leftBound (rightBound:nextBounds) = do
          g  <- getRandomR (leftBound, rightBound - len beta)
          gs <- shiftFrom betas (g + len beta) nextBounds
          return $ g : gs
        shiftFrom [] _ [] = return []
        shiftFrom _ _ _ = error "Norman blew the shift"

randomizePlacementNoAllocation
  :: RandomGen r => [BetaStrand] -> Placement -> Int -> Rand r Placement
randomizePlacementNoAllocation betas oldp maxRight = shiftFrom betas 0 (tail oldp)
  where shiftFrom (beta:betas) leftBound rights = do
          g  <- getRandomR (leftBound, rightBound - len beta)
          gs <- shiftFrom betas (g + len beta) nextBounds
          return $ g : gs
            where (rightBound, nextBounds) =
                    case rights of []     -> (maxRight, error "shift bounds")
                                   (b:bs) -> (b, bs)
        shiftFrom [] _ _ = return []

rightBound :: Placement -> Int -> QuerySequence -> Int
rightBound oldp i query = if succ i < length oldp then oldp !! succ i
                          else V.length query
       
betaRange :: QuerySequence -> [BetaStrand] -> Placement -> Int -> Int -> (Int, Int)
betaRange query betas oldp lastGuess i =
  (leftBound, rightBound oldp i query - len (betas !! i))
  where leftBound  = if i == 0 then 0
                     else len (betas !! pred i) + lastGuess
