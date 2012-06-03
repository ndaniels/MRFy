{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses #-}
module BetterLazySearchModel
       ( Scorer(..)
       , Age
       , Seed
       , SearchStrategy(..)
       , search', originalSearch
       )

where
  
import LazySearchModel (RandomStream(..), Scorer, Age, Seed, ScoredPopulation)
import qualified SearchModel as S
import Score
import Viterbi
--------------------------------------------------------

-- @ start movequality.tex
data MoveQuality = Movement | Progress
-- @ end movequality.tex
data SearchStrategy placement = 
 SS { gen0    :: Seed -> ScoredPopulation placement
    , nextGen :: Seed -> ScoredPopulation placement
                      -> ScoredPopulation placement
    , quality :: forall a . Seed -> History a -> MoveQuality
       -- ^ Precondition: history must be nonempty.
      -- @quality s 
       -- of @solns@ that contains an approved solution
    }

type SearchStop a = [Aged (ScoredPopulation a)] -> (Scored a, S.History a)

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
search' :: forall placement r
        . RandomStream r
       => SearchStrategy placement
       -> SearchStop placement
       -> r
       -> (Scored placement, S.History placement)
search' strat finish rand = (finish . approvedPops strat r 0 . gen0 strat) s0
 where (s0, r) = takeSeed rand
-- @ end search.tex

-- TODO keep a Scored (Age, Placement) to support Simulated Annealing
-- otherwise, need an out of band "best ever" updated at every step
-- this is also necessary to prevent SimAn from thinking it's improving
-- when in fact it isn't.

-------------------------------------------------------------------------------
--
-- code to adapt the original search function
--


originalSearch :: forall placement  r
               .  RandomStream r
               => S.SearchStrategy placement 
               -> Scorer placement 
               -> r
               -> (Scored placement, S.History placement)
originalSearch ostrat score r = search' newstrat stop r
  where (newstrat, stop) = adapt ostrat score

adapt :: S.SearchStrategy a -> Scorer a -> (SearchStrategy a, SearchStop a)
adapt ss score = (SS g0 nx app, stop S.emptyHistory)
  where g0 seed = map score (S.gen0 ss seed)
        nx = flip (S.nextGen ss) score
        app seeds age pops = scanForGood S.emptyHistory (zip3 seeds [age..] pops)
          where scanForGood older ((seed, age, pop) : pops) =
                  if S.accept ss seed older age then
                    [pop]
                  else
                    pop : scanForGood ((minimum pop,age) `S.hcons` older) pops
        stop older (Aged pop age : pops) =
          if S.quit ss older age then
            (minimum pop, older)
          else
            stop ((minimum pop, age) `S.hcons` older) pops

