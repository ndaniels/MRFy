{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, RankNTypes, NamedFieldPuns, ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module LazySearchModel
       ( Age
       , Aged(..), unAged, ageOf
       , History(..), unHistory, hcons, emptyHistory, historySolution
                    , extendUsefulHistory
       , scoreUtility
       , Utility(..), isUseless
       , SearchGen(..), SearchStop, SearchStrategy(..), searchStrategy
       , FullSearchStrategy(..), fullSearchStrategy, fullSearch
       , SearchDelta(..)
       , search
       , AUS
       )

where
  
import Control.Monad.Random
import Control.Monad

import Score
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
data SearchStrategy a gen =
  SS { searchGen  :: SearchGen a gen
     , searchStop :: SearchStop a }
-- @ start fullstrat.tex

-- Why doesn't the generator take a random thing and produce an
-- infinite list of scored populations?  Because that would imply that
-- *every* population is useful.
--
--
-- @ start strategy.tex
data SearchGen placement gen = 
 SG { gen0    :: Rand gen (Scored placement)
    , nextGen :: Scored placement -> Rand gen (Scored placement)
    , utility :: forall a . SearchDelta a -> Rand gen (Utility (Scored a))
    }
-- @ end strategy.tex
 -- ^ utility returns the *younger* item in the delta

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
searchStrategy :: (Rand gen (Scored placement))
               -> (Scored placement -> Rand gen (Scored placement))
               -> (forall a . SearchDelta a -> Rand gen (Utility (Scored a)))
               -> SearchStop placement
               -> SearchStrategy placement gen
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


scoreUtility :: SearchDelta a -> Rand gen (Utility (Scored a))
scoreUtility (SearchDelta { younger, older }) = return $
  if scoreOf younger < scoreOf older then Useful younger else Useless
                                         
instance Functor Utility where 
  fmap f (Useful a) = Useful (f a)
  fmap _ Useless    = Useless

isUseless :: Utility a -> Bool
isUseless Useless    = True
isUseless (Useful _) = False

instance Ord (History a) where
  compare (History h1) (History h2) = compare (map unAged h1) (map unAged h2)
    -- ^ Histories are compared by the score of the youngest element.

-- @ start everygen.tex
everyGen :: forall a r
         .  SearchGen a r
         -> Age
         -> Scored a
         -> Rand r [Aged (Utility (Scored a))]
everyGen ss age startPop = do
  children <- mapM (nextGen ss) (repeat startPop)
  moves    <- zipWithM agedUtility children [succ age..]
  let (useless, Aged (Useful newPop) newAge : _) = span (isUseless . unAged) moves
  nextGens <- everyGen ss newAge newPop
  return $ Aged (Useful startPop) age : useless ++ nextGens
  where agedUtility :: Scored a -> Age -> Rand r (Aged (Utility (Scored a)))
        agedUtility pop age = utility ss delta >>= \u -> return $ Aged u age
         where delta = SearchDelta { older = startPop, younger = pop, youngerAge = age }
-- @ end everygen.tex

  
--------------------------------------------------------
-- @ start search.tex
search :: forall placement r
       .  SearchStrategy placement r
       -> Rand r (History placement)
search (SS strat test) = fmap test . everyGen strat 0 =<< gen0 strat
-- @ end search.tex

data FullSearchStrategy placement gen =
  forall a . FSS { fssGen :: SearchGen a gen
                 , fssStop :: SearchStop a
                 , fssBest :: a -> placement }

-- @ start fullsearch.tex
fullSearch :: FullSearchStrategy a r -> Rand r (History a)
fullSearch (FSS gen stop best) = fmap (fmap best . stop) . everyGen gen 0 =<< gen0 gen
-- @ end fullsearch.tex        


fullSearchStrategy :: (Rand gen (Scored placement))
                   -> (Scored placement -> Rand gen (Scored placement))
                   -> (forall a . SearchDelta a -> Rand gen (Utility (Scored a)))
                   -> SearchStop placement
                   -> (placement -> answer)
                   -> FullSearchStrategy answer gen
fullSearchStrategy g0 n u s b = FSS (SG g0 n u) s b



-- TODO keep a Scored (Age, Placement) to support Simulated Annealing
-- otherwise, need an out of band "best ever" updated at every step
-- this is also necessary to prevent SimAn from thinking it's improving
-- when in fact it isn't.
