{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, RankNTypes, NamedFieldPuns #-}
module LazySearchModel
       ( Age
       , Seed
       , Aged(..), unAged, ageOf
       , History(..), hcons, emptyHistory, historySolution, extendUsefulHistory
       , scoreUtility
       , Utility(..), isUseless, consUseful
       , SearchGen(..), SearchStop, SearchStrategy(..), searchStrategy
       , SearchDelta(..)
       , search
       , AUS
       )

where
  
import System.Random
import qualified System.Random as R

import Score
import Viterbi
--------------------------------------------------------

{-

Preamble
--------
Starting from a good initial state, we generate a sequence of
successor states using random numbers, aka `Seed`s.   A new state
may be "useful" or "useless".  A useful state becomes the starting
point for a new search; a useless state is eventually discarded.

Both useful and useless states have *ages*; the age of a state is the
number of states (both useful and useless) that have preceded it in
the search.

Every state is assigned a *score*; the utility of a state is solely a
function of its age and score.

-}


-- @ start movequality.tex
data Utility a = Useful a | Useless
-- @ end movequality.tex
instance Functor Utility where
  fmap f (Useful a) = Useful (f a)
  fmap f Useless    = Useless

isUseless :: Utility a -> Bool
isUseless Useless    = True
isUseless (Useful _) = False

consUseful :: Utility a -> [a] -> [a]
Useless  `consUseful` as = as
Useful a `consUseful` as = a : as

-- @ start delta.tex
data SearchDelta a
  = SearchDelta { younger :: Scored a
                , older   :: Scored a
                , youngerAge :: Age }
-- @ end delta.tex

-- @ start strategy.tex
type Age  = Int -- number of generations explored
type Seed = Int -- source of stochastic variation

type ScoredPopulation a = [Scored a]
data SearchGen placement = 
 SG { gen0    :: Seed -> ScoredPopulation placement
    , nextGen :: Seed -> ScoredPopulation placement
                      -> ScoredPopulation placement
    , utility :: forall a . Seed -> SearchDelta a -> Utility a
    }

data Aged a = Aged a Age
  deriving (Show, Eq, Ord)
type SearchStop a = [Aged (Utility (Scored a))] -> History a
-- @ end strategy.tex
type AUS a = Aged (Utility (Scored a))
data SearchStrategy a = SS { searchGen :: SearchGen a, searchStop :: SearchStop a }
searchStrategy :: (Seed -> ScoredPopulation placement)
               -> (Seed -> ScoredPopulation placement -> ScoredPopulation placement)
               -> (forall a . Seed -> SearchDelta a -> Utility a)
               -> SearchStop placement
               -> SearchStrategy placement
searchStrategy g0 n u s = SS (SG g0 n u) s


scoreUtility :: seed -> SearchDelta a -> Utility a
scoreUtility _ (SearchDelta { younger, older }) = 
  if scoreOf younger < scoreOf older then Useful (unScored younger) else Useless


extendUsefulHistory :: AUS a -> History a -> History a
extendUsefulHistory (Aged Useless _) h = h
extendUsefulHistory (Aged (Useful a) age) h = Aged a age `hcons` h
                                         


instance Functor Aged where
  fmap f (Aged a age) = Aged (f a) age
unAged :: Aged a -> a
unAged (Aged a _) = a
ageOf :: Aged a -> Age
ageOf (Aged _ age) = age

split3 :: RandomGen r => r -> (r, r, r)
split3 r = let { (r0, r') = R.split r; (r1, r2) = R.split r' }
           in  (r0, r1, r2)


data History placement = History { unHistory :: [Aged (Scored placement)] }
  deriving (Show, Eq)
hcons :: Aged (Scored placement) -> History placement -> History placement
hcons a (History as) = History (a:as)
emptyHistory :: History a
emptyHistory = History []
historySolution :: History a -> Scored a
historySolution (History (asp : _)) = unAged asp
historySolution _ = error "solution from empty history"

instance Ord (History a) where
  compare (History h1) (History h2) = compare (map unAged h1) (map unAged h2)
    -- ^ Histories are compared by the score of the youngest element.

-- @ start children.tex
children :: RandomGen r
         => SearchGen a -> r -> ScoredPopulation a
         -> [ScoredPopulation a]
children ss r pop = map (flip (nextGen ss) pop) (R.randoms r)
-- @ end children.tex

everyGen :: forall r a .  RandomGen r
         => SearchGen a
         -> r
         -> Age
         -> ScoredPopulation a
         -> [Aged (Utility (ScoredPopulation a))]
everyGen ss r age startPop =
    Aged (Useful startPop) age : uselessMoves ++ everyGen ss r0 newAge newPop
  where
    (r0, r1, r2) = split3 r
    kids = zipWith3 decorate (children ss r1 startPop) (R.randoms r2) [succ age..]
    (uselessMoves, Aged (Useful newPop) newAge : _) = span (isUseless . unAged) kids
    decorate :: ScoredPopulation a -> Seed -> Age
             -> Aged (Utility (ScoredPopulation a))
    decorate pop seed newAge = Aged (fmap (const pop) (utility ss seed delta)) newAge
         where delta = SearchDelta { older      = minimum startPop
                                   , younger    = minimum pop
                                   , youngerAge = newAge }

  
--------------------------------------------------------
-- @ start search.tex
search :: forall placement r
       .  RandomGen r
       => SearchStrategy placement
       -> r
       -> History placement
search (SS strat test) rand =
   (test . (map . fmap . fmap) minimum . everyGen strat r 0 . gen0 strat) s0
 where (s0, r) = R.next rand
-- @ end search.tex


-- TODO keep a Scored (Age, Placement) to support Simulated Annealing
-- otherwise, need an out of band "best ever" updated at every step
-- this is also necessary to prevent SimAn from thinking it's improving
-- when in fact it isn't.
