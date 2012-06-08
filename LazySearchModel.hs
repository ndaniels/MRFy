{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, RankNTypes, NamedFieldPuns #-}
module LazySearchModel
       ( Age
       , Seed
       , Aged(..), unAged, ageOf
       , History(..), unHistory, hcons, emptyHistory, historySolution
                    , extendUsefulHistory
       , scoreUtility
       , Utility(..), isUseless
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

-------------- Type definitions for central concepts ----------
-- @ start strategy.tex
type Seed = Int -- source of stochastic variation
-- @ end strategy.tex

-- @ start strategy.tex
type Age  = Int -- number of generations explored
-- @ end strategy.tex

-- @ start utility.tex
data Utility a = Useful a | Useless
-- @ end utility.tex

-- | A @SearchDelta@ is used to make a decision about
-- whether a younger state is useless.  The decision
-- uses the score of the most recent useful state
-- (aka the @older@ state) as well as the score and
-- age of the state under scrutiny (the @younger@ state).
-- @ start delta.tex
data SearchDelta a
  = SearchDelta { older   :: Scored a
                , younger :: Scored a
                , youngerAge :: Age }
-- @ end delta.tex

-- | Many internal decisions are made based on age,
-- so we provide a way to tag any value with its age.
-- @ start aged.tex
data Aged a = Aged a Age
-- @ end aged.tex
  deriving (Show, Eq, Ord)
unAged :: Aged a -> a
unAged (Aged a _) = a
ageOf :: Aged a -> Age
ageOf (Aged _ age) = age



--------------- Search strategy ---------------------------
--
-- In general, a search strategy contains not just a single
-- placement but a population of placements.   As advocated 
-- by Hughes, a strategy is divided into two parts: generate
-- and (stop) test.
-- @ start fullstrat.tex
data SearchStrategy a =
  SS { searchGen  :: SearchGen a
     , searchStop :: SearchStop a }
-- @ start fullstrat.tex

-- Why doesn't the generator take a random thing and produce an
-- infinite list of scored populations?  Because that would imply that
-- *every* population is useful.
--
--
-- @ start strategy.tex
data SearchGen placement = 
 SG { gen0    :: Seed -> Scored placement
    , nextGen :: Seed -> Scored placement
                      -> Scored placement
    , utility :: forall a .
                 Seed -> SearchDelta a -> Utility a
    }
-- @ end strategy.tex

 -- | In a just world, a search would produce an infinite list of
 -- useful states tagged with score and age, and the stop function
 -- would grab a prefix.  Unfortunately, to guarantee productivity, we
 -- must include useless states.
 --
 -- A simple stop function would take this list and produce a result
 -- (value of type @a@) or perhaps a @Scored a@.  But because we are
 -- doing research into the convergence properties of various search
 -- strategies, we want to retain information about *all* useful
 -- states.  That is the role of @History@.  The list is always
 -- finite, and the youngest solution is at the beginning.
 --
 -- XXX this whole 'Age' thing is bogus.  Younger values have *larger* Ages!
 -- We need a short word meaning 'birthday' or 'date of manufacture'.
 
-- @ start history.tex
data History placement = History [Aged (Scored placement)]
-- @ end history.tex
  deriving (Show, Eq)
unHistory :: History a -> [Aged (Scored a)]
unHistory (History a) = a

-- | A stop function converts an infinite sequence of states into a
-- history.  It requires as a precondition that the input be infinite
-- and that the first state be useful.
-- @ start stop.tex
type SearchStop a = [Aged (Utility (Scored a))] -> History a
-- @ end stop.tex


-- | A constructor for search strategies.  Its role is to take
-- four flat arguments instead of a tree.  (Not sure this is a 
-- good idea.)
searchStrategy :: (Seed -> Scored placement)
               -> (Seed -> Scored placement -> Scored placement)
               -> (forall a . Seed -> SearchDelta a -> Utility a)
               -> SearchStop placement
               -> SearchStrategy placement
searchStrategy g0 n u s = SS (SG g0 n u) s



-------------- History ------------------------
--
-- If we 

instance Functor History where
  fmap f = History . (fmap . fmap . fmap) f . unHistory

-- data History placement = History [Aged (Scored placement)]



hcons :: Aged (Scored placement) -> History placement -> History placement
hcons a (History as) = History (a:as)
emptyHistory :: History a
emptyHistory = History []
historySolution :: History a -> Scored a
historySolution (History (asp : _)) = unAged asp
historySolution _ = error "solution from empty history"

extendUsefulHistory :: AUS a -> History a -> History a
extendUsefulHistory (Aged Useless _) h = h
extendUsefulHistory (Aged (Useful a) age) h = Aged a age `hcons` h




type AUS a = Aged (Utility (Scored a))


instance Functor Aged where
  fmap f (Aged a age) = Aged (f a) age


scoreUtility :: seed -> SearchDelta a -> Utility a
scoreUtility _ (SearchDelta { younger, older }) = 
  if scoreOf younger < scoreOf older then Useful (unScored younger) else Useless
                                         
instance Functor Utility where 
  fmap f (Useful a) = Useful (f a)
  fmap f Useless    = Useless

isUseless :: Utility a -> Bool
isUseless Useless    = True
isUseless (Useful _) = False



split3 :: RandomGen r => r -> (r, r, r)
split3 r = let { (r0, r') = R.split r; (r1, r2) = R.split r' }
           in  (r0, r1, r2)



instance Ord (History a) where
  compare (History h1) (History h2) = compare (map unAged h1) (map unAged h2)
    -- ^ Histories are compared by the score of the youngest element.

-- @ start children.tex
children :: RandomGen r
         => SearchGen a -> r -> Scored a
         -> [Scored a]
children ss r pop = map (flip (nextGen ss) pop) (R.randoms r)
-- @ end children.tex

everyGen :: forall r a .  RandomGen r
         => SearchGen a
         -> r
         -> Age
         -> Scored a
         -> [Aged (Utility (Scored a))]
everyGen ss r age startPop =
    Aged (Useful startPop) age : uselessMoves ++ everyGen ss r0 newAge newPop
  where
    (r0, r1, r2) = split3 r
    kids = zipWith3 decorate (children ss r1 startPop) (R.randoms r2) [succ age..]
    (uselessMoves, Aged (Useful newPop) newAge : _) = span (isUseless . unAged) kids
    decorate :: Scored a -> Seed -> Age
             -> Aged (Utility (Scored a))
    decorate pop seed newAge = Aged (fmap (const pop) (utility ss seed delta)) newAge
         where delta = SearchDelta { older      = startPop
                                   , younger    = pop
                                   , youngerAge = newAge }

  
--------------------------------------------------------
-- @ start search.tex
search :: forall placement r
       .  RandomGen r
       => SearchStrategy placement
       -> r
       -> History placement
search (SS strat test) rand = (test . everyGen strat r 0 . gen0 strat) s0
 where (s0, r) = R.next rand
-- @ end search.tex


-- TODO keep a Scored (Age, Placement) to support Simulated Annealing
-- otherwise, need an out of band "best ever" updated at every step
-- this is also necessary to prevent SimAn from thinking it's improving
-- when in fact it isn't.
