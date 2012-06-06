{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, RankNTypes #-}
module BetterLazySearchModel
       ( Scorer(..)
       , Age
       , Seed
       , SearchStrategy(..)
       , search', originalSearch
       )

where
  
import LazySearchModel (RandomStream(..), Scorer, Age, Seed)
import qualified LazySearchModel as S
import Score
import Viterbi
--------------------------------------------------------

type ScoredPopulation a = [Scored a]

-- @ start popstream.tex
data SearchStream a = Progress a Age (SearchStream a)
                    | Movement   Age (SearchStream a)
-- @ end popstream.tex

data SearchStrategy placement = 
 SS { gen0    :: Seed -> ScoredPopulation placement
    , nextGen :: Seed -> ScoredPopulation placement
                      -> ScoredPopulation placement
    , quality :: forall a . Seed -> S.SearchDelta a
              -> SearchStream a -> SearchStream a
    }

type SearchStop a = SearchStream (ScoredPopulation a) -> S.History a

children :: RandomStream r
         => SearchStrategy a
         -> r
         -> ScoredPopulation a
         -> [ScoredPopulation a]
children ss r pop = map (flip (nextGen ss) pop) (listRands r)

everyGen :: forall r a
         .  RandomStream r
         => SearchStrategy a
         -> r
         -> Age
         -> ScoredPopulation a
         -> SearchStream (ScoredPopulation a)
everyGen ss r age startPop =
    Progress startPop age $ movementPrefix $ everyGen ss r0 newAge newPop
  where
    (r0, r1, r2) = splitRand3 r
    kids = zipWith3 decorate (children ss r1 startPop) (listRands r2) [succ age..]
    (movementPrefix, Progress newPop newAge _) = stripPrefix id (toStream kids)
      where toStream (kid : kids) = kid (toStream kids)
            stripPrefix pre (ss @ Progress { }) = (pre, ss)
            stripPrefix pre (Movement age ss) = stripPrefix (pre . Movement age) ss

    decorate :: ScoredPopulation a -> Seed -> Age
             -> SearchStream (ScoredPopulation a)
             -> SearchStream (ScoredPopulation a)
    decorate pop seed newAge = quality ss seed (undefined delta)
         where delta = S.SearchDelta { S.older   = (minimum startPop, age)
                                     , S.younger = (minimum pop, newAge) }
          -- Perhaps this code makes it a bit clearer what the delta is --NR
    

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
       -> S.History placement
search' strat finish rand = (finish . everyGen strat r 0 . gen0 strat) s0
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
originalSearch ostrat score r = split $ search' newstrat stop r
  where (newstrat, stop) = adapt ostrat score
        split hist @ (S.History ((pop, _age) : _)) = (pop, hist)

adapt :: S.SearchStrategy a -> Scorer a -> (SearchStrategy a, SearchStop a)
adapt ss score = (SS g0 nx qual, stop S.emptyHistory)
  where g0 seed = map score (S.gen0 ss seed)
        nx = flip (S.nextGen ss) score

        qual seed delta =
          if S.accept ss seed history age then Progress pop age else Movement age
            where history = S.History [S.younger delta, S.older delta]
                  (pop, age) = S.younger delta

        stop older (Movement age pops) =
          if S.quit ss older age then older else stop older pops
{-
        stop older (Progress pop age pops) =
          if S.quit ss older age then newhist else stop newhist pops
              where newhist = minimum pop `S.hcons` older
-}

