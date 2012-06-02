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
  splitRand3 :: r -> (r, r, r)
  splitRand3 r = let { (r0, r') = splitRand r; (r1, r2) = splitRand r' }
                 in  (r0, r1, r2)

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
{-
class SearchStrategy s a where
  gen0    :: s -> Seed -> ScoredPopulation a
  nextGen :: s -> Seed -> ScoredPopulation a -> ScoredPopulation a
  children :: RandomStream r => s -> r -> ScoredPopulation a -> [ScoredPopulation a]
  children s r a = map (flip (nextGen s) a) (listRands r)
  approval :: s -> Seed -> History a -> Age -> Approval
-- @ end strategy.tex
-}
data SearchStrategy placement = 
 SS { gen0    :: Seed -> ScoredPopulation placement
    , nextGen :: Seed -> ScoredPopulation placement
                      -> ScoredPopulation placement
    , toApproved :: [Seed] -> Age
                 -> [ScoredPopulation placement]
                 -> [ScoredPopulation placement]
       -- ^ @toApproved s age solns@ returns the smallest prefix
       -- of @solns@ that contains an approved solution
    }

type SearchStop a = [Aged (ScoredPopulation a)] -> (Scored a, History a)

children :: RandomStream r
         => SearchStrategy a
         -> r
         -> ScoredPopulation a
         -> [ScoredPopulation a]
children ss r pop = map (flip (nextGen ss) pop) (listRands r)

approvedPops :: RandomStream r
             => SearchStrategy a
             -> r
             -> Age
             -> ScoredPopulation a
             -> [Aged (ScoredPopulation a)]
approvedPops ss r age startPop =
    Aged startPop age : approvedPops ss r0 (age + length kids) (last kids)
  where kids = toApproved ss (listRands r1) age $ children ss r2 startPop
        (r0, r1, r2) = splitRand3 r

data Aged a = Aged a Age
instance Functor Aged where
  fmap f (Aged a age) = Aged (f a) age
  
--------------------------------------------------------
-- @ start search.tex
search :: forall placement r
        . RandomStream r
       => SearchStrategy placement
       -> SearchStop placement
       -> r
       -> (Scored placement, History placement)
search strat finish rand = (finish . approvedPops strat r 0 . gen0 strat) s0
 where (s0, r) = takeSeed rand
-- @ end search.tex

-- TODO keep a Scored (Age, Placement) to support Simulated Annealing
-- otherwise, need an out of band "best ever" updated at every step
-- this is also necessary to prevent SimAn from thinking it's improving
-- when in fact it isn't.


data History placement = History [(Scored placement, Age)]
  deriving (Show, Eq, Ord)
  -- XXX TODO 
hmin :: History placement -> (Scored placement, Age)
hmin (History as) = minimum as
hcons :: (Scored placement, Age) -> History placement -> History placement
hcons a (History as) = History (a:as)
hmap :: ((Scored placement, Age) -> b) -> (History placement) -> [b]
hmap f (History as) = map f as

