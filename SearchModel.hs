{-# LANGUAGE ScopedTypeVariables, BangPatterns, RankNTypes #-}
module SearchModel
       ( Scorer(..)
       , Age
       , Seed
       , SearchStrategy(..)
       , History(..), hcons, emptyHistory
       , ShortHistory(..)
       , histProgresses, scoreProgresses
       , search
       )

where

import Debug.Trace
  
import Score
import Viterbi
--------------------------------------------------------

-- @ start scoredecl.tex
type Scorer placement = placement -> Scored placement
-- @ end scoredecl.tex
-- @ start strategy.tex
type Age  = Int -- number of generations explored
type Seed = Int -- source of stochastic variation
data History placement = History { unHistory :: [(Scored placement, Age)] }
  deriving (Show, Eq, Ord)
hcons :: (Scored placement, Age) -> History placement -> History placement
hcons a (History as) = History (a:as)
emptyHistory :: History a
emptyHistory = History []

-- is there a better name for seed?
data SearchStrategy placement = 
 SS { gen0    :: Seed -> [placement]
    , nextGen :: Seed -> Scorer placement
              -> [Scored placement] -> [Scored placement]
    , accept  :: forall a . Seed -> History a -> Age -> Bool
    , quit    :: forall a .         History a -> Age -> Bool
    }
-- @ end strategy.tex


data ShortHistory a = ShortHistory { younger :: (Scored a, Age)
                                   , older   :: (Scored a, Age)
                                   }
histProgresses :: (Seed -> ShortHistory a -> Bool)
               -> Seed -> History a -> Age -> Bool
histProgresses progress seed (History scores) age = ok scores
  where ok [] = error "asked about scores in an empty history"
        ok [_] = True
        ok (s1:s2:_) =
          if snd s1 == age then
            progress seed $ ShortHistory { younger = s1, older = s2 }
          else
            error "age passed to accept function is inconsistent with history"

scoreProgresses :: Seed -> ShortHistory a -> Bool
scoreProgresses _ h = (scoreOf . fst . younger) h < (scoreOf . fst . older) h

--------------------------------------------------------
-- @ start search.tex
search :: forall placement 
        . SearchStrategy placement 
       -> Scorer placement 
       -> [Seed]
       -> (Scored placement, History placement)
search strat scorer (s0:seeds) = runFrom seeds firstGen (History []) 0
 where
  firstGen = map scorer $ gen0 strat s0
  runFrom :: [Seed] -> [Scored placement] -> History placement
          -> Age -> (Scored placement, History placement)
  runFrom (s1:s2:seeds') oldPop oldHist age =
    let trialPop  = nextGen strat s1 scorer oldPop
        trialHist = (winner, age) `hcons` oldHist
        winner = minimum trialPop
        (newPop, newHist) =
          if accept strat s2 trialHist age then
            (trialPop, trialHist)
          else
            (oldPop, oldHist)
    in  if quit strat newHist age then -- TODO quit must change: consider best-ever, convergence
          (fst $ minimum (unHistory newHist), newHist) 
        else
          runFrom seeds' newPop newHist (age + 1)
-- @ end search.tex

-- TODO keep a Scored (Age, Placement) to support Simulated Annealing
-- otherwise, need an out of band "best ever" updated at every step
-- this is also necessary to prevent SimAn from thinking it's improving
-- when in fact it isn't.
