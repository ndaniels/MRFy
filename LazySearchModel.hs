{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, RankNTypes, NamedFieldPuns, ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module LazySearchModel
       ( CCost
       , CCosted(..), unCCosted, ccostOf
       , History(..), unHistory, hcons, emptyHistory, historySolution
                    , extendUsefulHistory
       , scoreUtility
       , Utility(..), isUseless
       , SearchGen(..), SearchStop, SearchStrategy(..), searchStrategy
       , FullSearchStrategy(..), fullSearchStrategy, fullSearch
       , Move(..)
       , search
       , AUS
       )

where
  
import Control.Applicative
import Control.Monad.LazyRandom
import Control.Monad
import Data.Function

import Score
--------------------------------------------------------

{-

Preamble
--------
Starting from a good initial state, we generate a sequence of
successor states using random numbers.  A new state may be "useful" or
"useless".  A useful state becomes the starting point for a new
search; a useless state is eventually discarded.

Both useful and useless states have *cumulative costs*; the cumulative
cost of a state is the number of states (both useful and useless) that
have preceded it in the search.

Every state is assigned a *score*; the utility of a state is solely a
function of its cumulative cost and score.

-}

-------------- Type definitions for central concepts ----------
-- @ start strategy.tex
type CCost  = Int -- cumulative cost of reaching this point
-- @ end strategy.tex

-- @ start utility.tex
data Utility a = Useful a | Useless
-- @ end utility.tex

-- | A @Move@ is used to make a decision about
-- whether a younger state is useless.  The decision
-- uses the score of the most recent useful state
-- (aka the @older@ state) as well as the score and
-- cumulative cost of the state under scrutiny (the @younger@ state).
-- @ start move.tex
data Move pt = Move { older        :: Scored pt
                    , younger      :: Scored pt
                    , youngerCCost :: CCost }
-- @ end move.tex

-- | Many internal decisions are made based on age,
-- so we provide a way to tag any value with its age.
-- @ start aged.tex
data CCosted a = CCosted a CCost
-- @ end aged.tex
  deriving (Show, Eq, Ord)
unCCosted :: CCosted a -> a
unCCosted (CCosted a _) = a
ccostOf :: CCosted a -> CCost
ccostOf (CCosted _ age) = age



--------------- Search strategy ---------------------------
--
-- In general, a search strategy contains not just a single
-- placement but a population of placements.   As advocated 
-- by Hughes, a strategy is divided into two parts: generate
-- and (stop) test.
-- @ start strat.tex
data SearchStrategy pt r =
  SS { searchGen  :: SearchGen pt r
     , searchStop :: SearchStop pt }
-- @ end strat.tex

-- Why doesn't the generator take a random thing and produce an
-- infinite list of scored populations?  Because that would imply that
-- *every* population is useful.
--
--

-- @ start gen.tex
data SearchGen pt r = 
 SG { pt0     :: Rand r (Scored pt)
    , nextPt  :: Scored pt -> Rand r (Scored pt)
    , utility :: Move pt -> Rand r (Utility (Scored pt))
    }
-- @ end gen.tex
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
 
-- @ start history.tex
newtype History placement = History [CCosted (Scored placement)]
-- @ end history.tex
  deriving (Show, Eq)
unHistory :: History a -> [CCosted (Scored a)]
unHistory (History a) = a

-- | A stop function converts an infinite sequence of states into a
-- history.  It requires as a precondition that the input be infinite
-- and that the first state be useful.
-- @ start stop.tex
type SearchStop pt =
       [CCosted (Utility (Scored pt))] -> History pt
-- @ end stop.tex


-- | A constructor for search strategies.  Its role is to take
-- four flat arguments instead of a tree.  (Not sure this is a 
-- good idea.)
searchStrategy :: (Rand gen (Scored placement))
               -> (Scored placement -> Rand gen (Scored placement))
               -> (forall a . Move a -> Rand gen (Utility (Scored a)))
               -> SearchStop placement
               -> SearchStrategy placement gen
searchStrategy g0 n u s = SS (SG g0 n u) s



-------------- History ------------------------
--
-- If we 

instance Functor History where
  fmap f = History . (fmap . fmap . fmap) f . unHistory

-- data History placement = History [CCosted (Scored placement)]



hcons :: CCosted (Scored placement) -> History placement -> History placement
hcons a (History as) = History (a:as)
emptyHistory :: History a
emptyHistory = History []
historySolution :: History a -> Scored a
historySolution (History (asp : _)) = unCCosted asp
historySolution _ = error "solution from empty history"

extendUsefulHistory :: AUS a -> History a -> History a
extendUsefulHistory (CCosted Useless _) h = h
extendUsefulHistory (CCosted (Useful a) ccost) h = CCosted a ccost `hcons` h

type AUS a = CCosted (Utility (Scored a))


instance Functor CCosted where
  fmap f (CCosted a ccost) = CCosted (f a) ccost


scoreUtility :: RandomGen gen => Move a -> Rand gen (Utility (Scored a))
scoreUtility (Move { younger, older }) = return $
  if scoreOf younger < scoreOf older then Useful younger else Useless
                                         
instance Functor Utility where 
  fmap f (Useful a) = Useful (f a)
  fmap _ Useless    = Useless

isUseless :: Utility a -> Bool
isUseless Useless    = True
isUseless (Useful _) = False

instance Ord (History a) where
  compare = compare `on` map unCCosted . unHistory
    -- ^ Histories are compared by the score of the youngest element.

-------------------------------------------------------V
-- @ start everygen.tex
everyPt :: RandomGen r
        => SearchGen pt r -> CCost -> Scored pt
        -> Rand r [CCosted (Utility (Scored pt))]
everyPt sg cost startPt = do
  successors <- mapM (nextPt sg) (repeat startPt)
  tagged <- zipWithM costedUtility successors [succ cost..]
  let (useless, CCosted (Useful newPt) newCost : _) =
                      span (isUseless . unCCosted) tagged
  (++) (CCosted (Useful startPt) cost : useless) <$>
                                  everyPt sg newCost newPt
 where costedUtility pt cost =
         utility sg move >>= \u -> return $ CCosted u cost
        where move = Move { older = startPt, younger = pt
                          , youngerCCost = cost }
-- @ end everygen.tex

  
--------------------------------------------------------
-- @ start search.tex
search :: RandomGen r => SearchGen pt r -> SearchStop pt
       -> Rand r (History pt)
search strat test =
  return . test =<< everyPt strat 0 =<< pt0 strat
-- @ end search.tex

data FullSearchStrategy placement r =
  forall pt . FSS { fssGen  :: SearchGen pt r
                  , fssStop :: SearchStop pt
                  , fssBest :: pt -> placement }

-- @ start fullsearch.tex
fullSearch :: RandomGen r => FullSearchStrategy a r -> Rand r (History a)
fullSearch (FSS gen stop best) = fmap (fmap best . stop) . everyPt gen 0 =<< pt0 gen
-- @ end fullsearch.tex        


fullSearchStrategy :: (Rand gen (Scored placement))
                   -> (Scored placement -> Rand gen (Scored placement))
                   -> (forall a . Move a -> Rand gen (Utility (Scored a)))
                   -> SearchStop placement
                   -> (placement -> answer)
                   -> FullSearchStrategy answer gen
fullSearchStrategy g0 n u s b = FSS (SG g0 n u) s b



-- TODO keep a Scored (CCost, Placement) to support Simulated Annealing
-- otherwise, need an out of band "best ever" updated at every step
-- this is also necessary to prevent SimAn from thinking it's improving
-- when in fact it isn't.
