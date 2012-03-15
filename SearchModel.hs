{-# LANGUAGE ScopedTypeVariables #-}
module SearchModel 
where
  
import Score
--------------------------------------------------------

-- @ start scoredecl.tex
type Scorer placement = placement -> Scored placement
-- @ end scoredecl.tex
-- @ start strategy.tex
type Age  = Int -- number of generations explored
type Seed = Int -- source of stochastic variation
-- is there a better name for seed?
data SearchStrategy placement = 
 SS { gen0    :: Seed -> [placement]
    , nextGen :: Seed -> Scorer placement
              -> [Scored placement] -> [Scored placement]
    , accept  :: Seed -> [Scored Age] -> Age -> Bool
    , quit    ::         [Scored Age] -> Age -> Bool
    }
-- @ end strategy.tex

--------------------------------------------------------
-- @ start search.tex
search :: forall placement 
        . SearchStrategy placement 
       -> Scorer placement 
       -> [Seed]
       -> (Scored placement, [Scored Age])
search strat scorer (s0:seeds) = runFrom seeds firstGen [] 0
 where
  firstGen = map scorer $ gen0 strat s0
  runFrom :: [Seed] -> [Scored placement] -> [Scored Age]
          -> Age -> (Scored placement, [Scored Age])
  runFrom (s1:s2:seeds) oldPop oldHist age =
    let trialPop  = nextGen strat s1 scorer oldPop
        trialHist = (fmap (const age) $ minimum trialPop)
                  : oldHist
        (newPop, newHist) =
          if accept strat s2 trialHist age then
            (trialPop, trialHist)
          else
            (oldPop, oldHist)
    in  if quit strat newHist age then
          (minimum newPop, newHist)
        else
          runFrom seeds newPop newHist (age + 1)
-- @ end search.tex
