{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module SearchModel
       ( Scorer(..)
       , Age
       , Seed
       , SearchStrategy(..), History(..)
       , search
       )

where

import Debug.Trace (trace)
  
import Score
import Viterbi
--------------------------------------------------------

-- @ start scoredecl.tex
type Scorer placement = placement -> Scored placement
-- @ end scoredecl.tex
-- @ start strategy.tex
type Age  = Int -- number of generations explored
type Seed = Int -- source of stochastic variation
data History placement = History [(Scored placement, Age)]
  deriving (Show, Eq, Ord)
hmin :: History placement -> (Scored placement, Age)
hmin (History as) = minimum as
hcons :: (Scored placement, Age) -> History placement -> History placement
hcons a (History as) = History (a:as)
hmap :: ((Scored placement, Age) -> b) -> (History placement) -> [b]
hmap f (History as) = map f as

-- is there a better name for seed?
data SearchStrategy placement = 
 SS { gen0    :: Seed -> [placement]
    , nextGen :: Seed -> Scorer placement
              -> [Scored placement] -> [Scored placement]
    , accept  :: Seed -> History placement -> Age -> Bool
    , quit    ::         History placement -> Age -> Bool
    }
-- @ end strategy.tex

--------------------------------------------------------
-- @ start search.tex
search :: forall placement
       . (Show placement)
       => SearchStrategy placement 
       -> Scorer placement 
       -> [Seed]
       -> (Scored placement, History placement)
search strat scorer (s0:seeds) = runFrom seeds firstGen (History []) 0
 where
  firstGen = map scorer $ gen0 strat s0
  runFrom :: [Seed] -> [Scored placement] -> History placement
          -> Age -> (Scored placement, History placement)
  runFrom (s1:s2:_) oldPop oldHist age =
    let trialPop  = nextGen strat s1 scorer oldPop
        trialHist = (winner, age) `hcons` oldHist
        winner = minimum trialPop
        (newPop, newHist) =
          if accept strat s2 trialHist age then
            (trialPop, trialHist)
          else
            (oldPop, oldHist)
    in  if quit strat newHist age then
          (fst $ hmin newHist, newHist) 
        else
          runFrom seeds newPop newHist (age + 1)
-- @ end search.tex

