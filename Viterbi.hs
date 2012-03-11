{-# LANGUAGE BangPatterns #-}
module Viterbi where

import Debug.Trace (trace)
-- import Debug.Trace.LocationTH (check) 
import qualified Data.MemoCombinators as Memo
import qualified Data.List as DL

import Beta
import HmmPlus
import Data.Vector
import Constants

type QuerySequence = Vector Int -- indices into Constants.amino
type Score = Double
type StatePath = [ HMMState ]

-- Remember, for states, 0 is a match, 1 is insertion and 2 is deletion.
-- It must be this way because the Pads parser does not support non-base
-- types as the parameter for an algebraic parser type. *sigh*
-- mat = 0 :: HMMState 
-- ins = 1 :: HMMState 
-- del = 2 :: HMMState 
-- beg = 3 :: HMMState 
-- end = 4 :: HMMState 
-- bmat = 5 :: HMMState 

type ScorePathCons a = a -> [a] -> [a]
type Result = (Score, StatePath)

unscore :: Scored a -> (Score, a)
unscore (Scored a x) = (x, a)

consPath :: ScorePathCons a
consPath x xs = x:xs

consNoPath :: ScorePathCons a
consNoPath _ _ = []

-- hasStart and hasEnd are (for now) for model-relative local alignment.
-- when we want to consider sequence-relative local alignment, we
-- will also need to consider better of seqLocal vs. modLocal
viterbi :: ScorePathCons HMMState -> (Bool, Bool) -> Alphabet -> QuerySequence -> HMM -> Result
viterbi pathCons (hasStart, hasEnd) alpha query hmm =
  if numNodes == 0 then
    (0.0, [])
  else
    flipSnd $ unscore $ viterbi' Mat 2 0
    -- flipSnd $ DL.minimum $ DL.map unscore $ 
    -- [viterbi' Mat (numNodes - 1) (seqlen - 1), 
     -- viterbi' Ins (numNodes - 1) (seqlen - 1), 
     -- viterbi' Del (numNodes - 1) (seqlen - 1) 
    -- ] DL.++ if hasEnd then [bestEnd] else [] 


  -- trace (show state DL.++ " " DL.++ show node DL.++ " " DL.++ show obs) $
  where viterbi' state j i = trace ((show state) DL.++ "::" DL.++ (show j) DL.++ "::" DL.++ (show i)) $ Memo.memo3 (Memo.arrayRange (Mat, End)) 
                                  (Memo.arrayRange (0, numNodes))
                                  (Memo.arrayRange (0, seqlen)) 
                                  viterbi'' state j i

        -- bestEnd = viterbi' End (numNodes - 1) (seqlen - 1) 

        -- we see observation obs with node at state
        flipSnd pair = (fst pair, DL.reverse $ snd pair)

        numNodes = Data.Vector.length $ hmm
        seqlen = Data.Vector.length query

        res i = query ! i

        eProb j i = emissionProb (matchEmissions $ hmm ! j) (res i)
        tProb f j = transProb hmm j f
        edge :: HMMState -> HMMState -> StateAcc
        edge Mat Mat = m_m
        edge Ins Mat = i_m
        edge Del Mat = d_m
        edge _   _   = error "unimplemnted or disallowed HMM edge"

        insertProb j i = emissionProb (insertionEmissions $ hmm ! j) (res i)
        matchProb  j i = emissionProb (matchEmissions     $ hmm ! j) (res i)

        disallowed = Scored [] maxProb

        --------------------------------------------------------
        -- @ start viterbi.tex -8
        vpaper' Mat j i = fmap (pathCons Mat)
          -- BLOWS THE STACK
          (eProb j i /+/ DL.minimum [from Mat, from Ins, from Del])
          -- DOES NOT BLOW THE STACK
          -- (eProb j i /+/ DL.minimum [from Mat, from Ins]) 
          -- DOES NOT BLOW THE STACK
          -- (eProb j i /+/ from Del) 
         where from prev = tProb (edge prev Mat) (j-1) /+/
                                       viterbi' prev (j-1) (i-1)
        -- @ end viterbi.tex

        -- node 1 and zeroth observation
        viterbi'' Mat 1 0 = Scored [Mat] (tProb m_m 0 + matchProb 1 0)
                            --           ^^^^^^^^^^^^^ is this right?  ---NR
                            -- we came from 'begin'
        viterbi'' Ins 1 0 = disallowed
        viterbi'' Del 1 0 = disallowed

        -- node 0 and zeroth observation, base of self-insert
        viterbi'' Mat 0 0 = disallowed
        viterbi'' Ins 0 0 = Scored [Ins] (tProb m_i 0 + insertProb 0 0)
        viterbi'' Del 0 0 = disallowed

        -- node 0 and no observations
        viterbi'' Mat 0 (-1) = Scored [] (tProb m_m 0)
        viterbi'' Ins 0 (-1) = disallowed
        viterbi'' Del 0 (-1) = disallowed

        -- node 0 but not zeroth observation
        viterbi'' Mat 0 i = disallowed
        viterbi'' Ins 0 i = fmap (pathCons Ins) $
                            (tProb i_i 0 + insertProb 0 i) /+/ viterbi' Ins 0 (i-1)
                            -- possible self-insert cycle

        viterbi'' Del 0 i = disallowed
        viterbi'' End 0 i = Scored [Mat] (tProb m_e 0)

        -- node 1 and no more observations (came from begin)
        viterbi'' Mat 1 (-1) = disallowed
        viterbi'' Ins 1 (-1) = disallowed
        viterbi'' Del 1 (-1) = Scored [Del] (tProb m_d 0) -- came from begin

        -- not node 1 yet, but not more observations (came from delete)
        viterbi'' Mat j (-1) = disallowed
        viterbi'' Ins j (-1) = disallowed
        viterbi'' Del j (-1) = fmap (pathCons Del) $
                               tProb d_d (j - 1) /+/ viterbi' Del (j - 1) (-1)

        -- consume an observation AND a node
        -- I think only this equation will change when
        -- we incorporate the begin-to-match code
        viterbi'' Mat j i = vpaper' Mat j i

        -- vpaper' Mat j i = fmap (pathCons Mat) 
          -- (eProb j i /+/ DL.minimum [from Mat, from Ins, from Del])  
         -- where from prev = tProb (edge prev Mat) (j-1) /+/ 
                                       -- viterbi' prev (j-1) (i-1) 

        -- match came from start                                            
        -- consume an observation but not a node
        viterbi'' Ins j i = fmap (pathCons Ins) $ eProb j i /+/
                              DL.minimum [from Mat, from Ins]
          where from prev = tProb (edge prev Ins) j /+/ viterbi' prev j (i - 1)

        -- consume a node but not an observation
        viterbi'' Del j i = fmap (pathCons Del) $
                              DL.minimum [from Mat, from Del]
          where from prev = tProb (edge prev Del) (j - 1) /+/
                              viterbi' prev (j - 1) i

        viterbi'' End j i = fmap (pathCons End) $ DL.minimum $
                              if j >= 2 then
                                [from Mat, from End]
                              else
                                [from Mat]
          where from prev = case prev of
                              Mat -> tProb (edge prev Mat) (j - 1) /+/
                                       viterbi' prev (j - 1) i
                              otherwise -> viterbi' prev (j - 1) i
                        -- for local to QUERY we would do j, i-1.

-- TODO seqLocal: consider the case where we consume obs, not state, for beg & end.

-- TODO preprocessing: convert hmm to array of nodes with the stateZero and insertZero stuff prepended
-- this will transform `node` below and `transProb` below

emissionProb :: Vector a -> Int -> a
emissionProb emissions residue = emissions ! residue

-- possible speedup: avoid this case analysis; substitute for maxProb in HmmPlus
transProb :: HMM -> Int -> StateAcc -> Double
transProb hmm nodenum state = case logProbability $ state (transitions (hmm ! nodenum)) of
                                   NonZero p -> p
                                   LogZero -> maxProb

-- @ start vscore.tex
data Scored a = Scored a Score
(/+/) :: Score -> Scored a -> Scored a
-- @ end vscore.tex

instance Eq (Scored a) where
  x == x' = scoreOf x == scoreOf x'
instance Ord (Scored a) where
  x < x' = scoreOf x < scoreOf x'

instance Functor Scored where
  fmap f (Scored a x) = Scored (f a) x

instance Show (Scored a) where
  show (Scored a x) = show x

scoreOf :: Scored a -> Score
scoreOf (Scored _ x) = x

infix /+/
x /+/ Scored a y = Scored a (x + y)

