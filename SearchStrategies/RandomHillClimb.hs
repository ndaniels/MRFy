{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module SearchStrategies.RandomHillClimb
       ( nss
       , randomizePlacement
       , initialize
       , mutate
       )
where

import Control.Applicative
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
  (takeByCCostGap (acceptableCCostGap searchP))
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
  scorer <$> randomizePlacement betas oldp (V.length query)

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
randomizePlacement betas oldp maxRight = shiftFrom betas 0 (tail oldp ++ [maxRight])
  where shiftFrom (beta:betas) leftBound (rightBound:nextBounds) = do
          g  <- getRandomR (leftBound, rightBound - len beta)
          gs <- shiftFrom betas (g + len beta) nextBounds
          return $ g : gs
        shiftFrom [] _ [] = return []
        shiftFrom _ _ _ = error "Norman blew the shift"
