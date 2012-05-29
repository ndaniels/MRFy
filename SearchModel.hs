{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module SearchModel
       ( Scorer(..)
       , Age
       , Seed
       , SearchStrategy(..)
       , search
       )

where
  
import Score
import Viterbi
--------------------------------------------------------

-- @ start scoredecl.tex
type Scorer placement = placement -> Scored placement
-- @ end scoredecl.tex
-- @ start strategy.tex
type Age  = Int -- number of generations explored
type Seed = Int -- source of stochastic variation
data History placement = History [Scored (placement, Age)]
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
        . SearchStrategy placement 
       -> Scorer placement 
       -> [Seed]
       -> (Scored placement, History placement) -- [Scored (placement, Age)]
search strat scorer (s0:seeds) = runFrom seeds firstGen (History []) 0
 where
  firstGen = map scorer $ gen0 strat s0
  runFrom :: [Seed] -> [Scored placement] -> History placement -- [Scored (placement, Age)]
          -> Age -> (Scored placement, History placement) -- [Scored (placement, Age)]
  runFrom (s1:s2:seeds) oldPop oldHist age =
    let trialPop  = nextGen strat s1 scorer oldPop
        trialHist = Scored (unScored $ winner, age) (scoreOf winner) -- must cons (placement, age)
                  : oldHist
        winner = minimum trialPop
        (newPop, newHist) =
          if accept strat s2 trialHist age then -- TODO accept just needs to deal with new history tuples
            (trialPop, trialHist)
          else
            (oldPop, oldHist)
    in  if quit strat newHist age then -- TODO quit must change: consider best-ever, convergence
          (minimum newPop, newHist) -- TODO this changes too: we want the minimum over newHist, 
                                    -- and map snd newHist
        else
          runFrom seeds newPop newHist (age + 1)
-- @ end search.tex

-- TODO keep a Scored (Age, Placement) to support Simulated Annealing
-- otherwise, need an out of band "best ever" updated at every step
-- this is also necessary to prevent SimAn from thinking it's improving
-- when in fact it isn't.
