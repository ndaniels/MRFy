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
import qualified SearchModel as S
import Score
import Viterbi
--------------------------------------------------------

type ScoredPopulation a = [Scored a]

-- @ start popstream.tex
data SearchStream a = Progress a (SearchStream a)
                    | Movement   (SearchStream a)
-- @ end popstream.tex

data SearchStrategy placement = 
 SS { gen0    :: Seed -> ScoredPopulation placement
    , nextGen :: Seed -> ScoredPopulation placement
                      -> ScoredPopulation placement
    , quality :: forall a . Seed -> S.ShortHistory a
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
         -> ScoredPopulation a
         -> SearchStream (ScoredPopulation a)
everyGen ss r age startPop =
    Progress startPop $ movementPrefix $ everyGen ss r0 newAge newPop
  where
    (r0, r1, r2) = splitRand3 r
    kids = zipWith3 decorate (children ss r1 startPop) (listRands r2) [succ age..]
    (movePrefix, ((Aged newPop newAge, Progress) : _)) = span (noProgress . snd) kids
    decorate :: ScoredPopulation a -> Seed -> Age
             -> SearchStream (ScoredPopulation a)
             -> SearchStream (ScoredPopulation a)
    decorate pop seed newAge = quality ss seed delta
         where delta = S.ShortHistory { S.older   = (minimum startPop, age)
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
       -> (Scored placement, S.History placement)
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
originalSearch ostrat score r = search' newstrat stop r
  where (newstrat, stop) = adapt ostrat score

adapt :: S.SearchStrategy a -> Scorer a -> (SearchStrategy a, SearchStop a)
adapt ss score = (SS g0 nx qual, split . stop S.emptyHistory)
  where g0 seed = map score (S.gen0 ss seed)
        nx = flip (S.nextGen ss) score

        qual seed delta =
          if S.accept ss seed history age then Progress else Movement
            where history = S.History [S.younger delta, S.older delta]
                  age = snd (S.younger delta)

        stop older ((Aged pop age, quality) : pops) =
          if S.quit ss older age then newhist else stop newhist pops
              where newhist =
                      case quality of Movement -> older
                                      Progress -> (minimum pop, age) `S.hcons` older
        split hist @ (S.History ((pop, _age) : _)) = (pop, hist)

