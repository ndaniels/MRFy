{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, RankNTypes #-}
module BetterLazySearchModel
       ( Scorer(..)
       , Age
       , Seed
       , SearchStrategy(..)
       , search', originalSearch
       )

where
  
import qualified System.Random as R

import LazySearchModel (listRands, split3, Scorer, Age, Seed)
import qualified SearchModel as S
import Score
import Viterbi
--------------------------------------------------------


-- @ start movequality.tex
data MoveUtility a = Useful a | Useless
-- @ end movequality.tex
instance Functor MoveUtility where
  fmap f (Useful a) = Useful (f a)
  fmap f Useless    = Useless

isUseless :: MoveUtility a -> Bool
isUseless Useless    = True
isUseless (Useful _) = False

-- @ start strategydecl.tex
type ScoredPopulation a = [Scored a]
data SearchStrategy placement = 
 SS { gen0    :: Seed -> ScoredPopulation placement
    , nextGen :: Seed -> ScoredPopulation placement
                      -> ScoredPopulation placement
    , utility :: forall a . Seed -> S.SearchDelta a -> MoveUtility a
    }

type SearchStop a = [Aged (MoveUtility (Scored a))] -> S.History a
-- @ end strategydecl.tex

class R.RandomGen r => RandomGen r

children :: RandomGen r
         => SearchStrategy a
         -> r
         -> ScoredPopulation a
         -> [ScoredPopulation a]
children ss r pop = map (flip (nextGen ss) pop) (listRands r)

everyGen :: forall r a
         .  RandomGen r
         => SearchStrategy a
         -> r
         -> Age
         -> ScoredPopulation a
         -> [(Aged (MoveUtility (ScoredPopulation a)))]
everyGen ss r age startPop =
    (Aged (Useful startPop) age) : uselessMoves ++ everyGen ss r0 newAge newPop
  where
    (r0, r1, r2) = split3 r
    kids = zipWith3 decorate (children ss r1 startPop) (listRands r2) [succ age..]
    (uselessMoves, Aged (Useful newPop) newAge : _) = span (isUseless . unAged) kids
    decorate :: ScoredPopulation a -> Seed -> Age
             -> Aged (MoveUtility (ScoredPopulation a))
    decorate pop seed newAge = Aged (fmap (const pop) (utility ss seed delta)) newAge
         where delta = S.SearchDelta { S.older   = minimum startPop
                                     , S.younger = minimum pop
                                     , S.youngerAge = newAge }

data Aged a = Aged { unAged :: a, age :: Age }
instance Functor Aged where
  fmap f (Aged a age) = Aged (f a) age
  
--------------------------------------------------------
-- @ start search.tex
search' :: forall placement r
        . RandomGen r
       => SearchStrategy placement
       -> SearchStop placement
       -> r
       -> S.History placement
search' strat finish rand = (finish . undefined . everyGen strat r 0 . gen0 strat) s0
 where (s0, r) = R.next rand
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
               .  RandomGen r
               => S.SearchStrategy placement 
               -> Scorer placement 
               -> r
               -> S.History placement
originalSearch ostrat score r = search' newstrat stop r
  where (newstrat, stop) = adapt ostrat score

adapt :: S.SearchStrategy a -> Scorer a -> (SearchStrategy a, SearchStop a)
adapt ss score = (SS g0 nx qual, stop S.emptyHistory)
  where g0 seed = map score (S.gen0 ss seed)
        nx = flip (S.nextGen ss) score

        qual seed delta =
          if S.accept ss seed history age then Useful (unScored (S.younger delta)) else Useless
            where history = S.History [ (S.younger delta, age)
                                      , (S.older delta,
                                         error "accept uses age of older state") ]
                  age = S.youngerAge delta

        stop older (Aged Useless age : pops) =
          if S.quit ss older age then older else stop older pops
        stop older (Aged (Useful best) age : pops) =
          if S.quit ss older age then newhist else stop newhist pops
              where newhist = (best, age) `S.hcons` older

