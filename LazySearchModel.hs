{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses #-}
module LazySearchModel
       ( Scorer(..)
       , Age
       , Seed
       , SearchStrategy(..), History(..)
--       , search
       )

where
  
import Score
import Viterbi
--------------------------------------------------------

-- @ start rand.tex
class RandomStream r where
  -- ^ must be used in linear fashion
  splitRand :: r -> (r, r)
  listRands :: r -> [Seed]
  takeSeed  :: r -> (Seed, r)
  takeSeed r = (s, r1)
    where (r0, r1) = splitRand r
          s : _    = listRands r0

-- @ start scoredecl.tex
type Scorer placement = placement -> Scored placement
-- @ end scoredecl.tex
-- @ start strategy.tex
type Age  = Int -- number of generations explored
type Seed = Int -- source of stochastic variation

data NextState = SiblingNext | ChildNext
data Approval = Accepted | Rejected
type ScoredPopulation a = [Scored a]
-- is there a better name for seed?
class SearchStrategy s a where
  gen0    :: s -> Seed -> ScoredPopulation a
  nextGen :: s -> Seed -> ScoredPopulation a -> ScoredPopulation a
  children :: RandomStream r => s -> r -> ScoredPopulation a -> [ScoredPopulation a]
  children s r a = map (flip (nextGen s) a) (listRands r)
  approval :: s -> Seed -> History a -> Age -> Approval
-- @ end strategy.tex
{- 
data SearchStrategy placement = 
 SS { gen0    :: Seed -> ScoredPopulation placement
    , nextGen :: Seed -> ScoredPopulation placement
                      -> ScoredPopulation placement
    , approval :: Seed -> History placement -> Age -> Approval
    }
-}

  


{-
--------------------------------------------------------
-- @ start search.tex
search :: forall placement, r
        . RandomStream r
       => SearchStrategy placement 
       -> [(Scored placement, History placement)]
search strat rand = runFrom seeds firstGen (History []) 0
 where
  (s0, rand') = takeSeed
  
  firstGen = gen0 strat s0
  runFrom :: r -> ScoredPopulation placement -> History placement
          -> Age -> [(Scored placement, History placement)]
  runFrom rand oldPop oldHist age =
    let children = map (flip (nextGen strat)) (listRands r1) oldPop
        trialHist = (winner, age) `hcons` oldHist
        winner = minimum trialPop
        (newPop, newHist) =
          if accept strat s2 trialHist age then
            (trialPop, trialHist)
          else
            (oldPop, oldHist)
    in  if quit strat newHist age then -- TODO quit must change: consider best-ever, convergence
          (fst $ hmin newHist, newHist) 
        else
          runFrom seeds newPop newHist (age + 1)
-- @ end search.tex

-- TODO keep a Scored (Age, Placement) to support Simulated Annealing
-- otherwise, need an out of band "best ever" updated at every step
-- this is also necessary to prevent SimAn from thinking it's improving
-- when in fact it isn't.
-}

data History placement = History [(Scored placement, Age)]
  deriving (Show, Eq, Ord)
hmin :: History placement -> (Scored placement, Age)
hmin (History as) = minimum as
hcons :: (Scored placement, Age) -> History placement -> History placement
hcons a (History as) = History (a:as)
hmap :: ((Scored placement, Age) -> b) -> (History placement) -> [b]
hmap f (History as) = map f as

