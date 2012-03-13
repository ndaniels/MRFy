{-# LANGUAGE BangPatterns #-}
module Viterbi where

import Debug.Trace (trace)
-- import Debug.Trace.LocationTH (check) 
import qualified Data.MemoCombinators as Memo
import qualified Data.List as DL

import Beta
import HmmPlus
import Data.Vector hiding (minimum, (++), map)
import Constants
import Score

type QuerySequence = Vector Int -- indices into Constants.amino
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

unscore :: Scored a -> (Score, a)
unscore (Scored a x) = (x, a)

consPath :: ScorePathCons a
consPath x xs = x:xs

consNoPath :: ScorePathCons a
consNoPath _ _ = []

type TProb = TransitionProbability
type TProbs = TransitionProbabilities

-- hasStart and hasEnd are (for now) for model-relative local alignment.
-- when we want to consider sequence-relative local alignment, we
-- will also need to consider better of seqLocal vs. modLocal
viterbi :: ScorePathCons HMMState -> (Bool, Bool) -> Alphabet -> QuerySequence -> HMM
        -> Scored StatePath
viterbi pathCons (hasStart, hasEnd) alpha query hmm =
  if numNodes == 0 then
    Scored [] negLogOne
  else
    myminimum $ [vee'' s (numNodes - 1) (seqlen - 1) | s <- [Mat, Ins, Del]] ++
              if hasEnd then [bestEnd] else []

  -- trace (show state ++ " " ++ show node ++ " " ++ show obs) $
  where 
        -- @ start memo.tex -8
        vee'' state j i =
          Memo.memo3 (Memo.arrayRange (Mat, End)) 
                     (Memo.arrayRange (0, numNodes))
                     (Memo.arrayRange (0, seqlen)) 
                     vee' state j i
        -- @ end memo.tex

        bestEnd = vee' End (numNodes - 1) (seqlen - 1)

        -- we see observation obs with node at state
        flipSnd pair = (fst pair, DL.reverse $ snd pair)

        numNodes = Data.Vector.length hmm
        seqlen   = Data.Vector.length query

        res i = query ! i

        extend :: HMMState -> Scored StatePath -> Scored StatePath
        extend s = fmap (pathCons s)

        aScore :: HMMState -> HMMState -> Int -> Score
        aScore = transScore hmm

        eScore :: HMMState -> Int -> Int -> Score
        eScore Mat j i = Score $ emissionProb (matchEmissions     $ hmm ! j) (res i)
        eScore Ins j i = Score $ emissionProb (insertionEmissions $ hmm ! j) (res i)
        eScore s   _ _ = error $ "State " ++ show s ++ " has no emission score"

        disallowed = Scored [] negLogZero -- outcome of zero likelihood

        vee' :: HMMState -> Int -> Int -> Scored [HMMState]

        -- node 1 and zeroth observation
        vee' Mat 1 0 = Scored [Mat] (aScore Mat Mat 0 + eScore Mat 1 0) -- from Beg
        vee' Ins 1 0 = disallowed
        vee' Del 1 0 = disallowed

        -- node 0 and zeroth observation, base of self-insert
        vee' Mat 0 0 = disallowed
        vee' Ins 0 0 = Scored [Ins] (aScore Mat Ins 0 + eScore Ins 0 0)
        vee' Del 0 0 = disallowed

        -- node 0 and no observations
        vee' Mat 0 (-1) = Scored [] (aScore Mat Mat 0)
        vee' Ins 0 (-1) = disallowed
        vee' Del 0 (-1) = disallowed

        -- node 0 but not zeroth observation
        vee' Mat 0 i = disallowed
        vee' Ins 0 i = extend Ins $    -- possible self-insert cycle
                       (aScore Ins Ins 0 + eScore Ins 0 i) /+/ vee'' Ins 0 (i-1)
        vee' Del 0 i = disallowed
        vee' End 0 i = Scored [Mat] (aScore Mat End 0)

        -- node 1 and no more observations (came from begin)
        vee' Mat 1 (-1) = disallowed
        vee' Ins 1 (-1) = disallowed
        vee' Del 1 (-1) = Scored [Del] (aScore Mat Del 0) -- came from begin

        -- not node 1 yet, but not more observations (came from delete)
        vee' Mat j (-1) = disallowed
        vee' Ins j (-1) = disallowed
        vee' Del j (-1) = extend Del $
                          aScore Del Del (j - 1) /+/ vee'' Del (j - 1) (-1)

        -- consume an observation AND a node
        -- I think only this equation will change when
        -- we incorporate the begin-to-match code
        --------------------------------------------------------
        -- @ start viterbi.tex -8
        vee' Mat j i = fmap (pathCons Mat)
          (eScore Mat j i /+/ myminimum (map from [Mat, Ins, Del]))
         where from prev =
                aScore prev Mat (j-1) /+/ vee'' prev (j-1) (i-1)
        -- @ end viterbi.tex
        vee' Ins j i = extend Ins (eScore Ins j i /+/ myminimum (map from [Mat, Ins]))
          where from prev = aScore prev Ins j /+/ vee'' prev j (i-1)

        -- consume a node but not an observation
        vee' Del j i = extend Del $ myminimum (map from [Mat, Del])
          where from prev = aScore prev Del (j-1) /+/ vee'' prev (j-1) i

        vee' End j i = extend End $ myminimum (map from preds)
          where preds = if j >= 2 then [Mat, End] else [Mat]
                from Mat  = aScore Mat Mat (j-1) /+/ vee'' Mat  (j-1) i
                from prev =                          vee'' prev (j-1) i
                        -- for local to QUERY we would do j, i-1.

-- TODO seqLocal: consider the case where we consume obs, not state, for beg & end.

-- TODO preprocessing: convert hmm to array of nodes with the stateZero and insertZero stuff prepended
-- this will transform `node` below and `transProb` below

emissionProb :: Vector a -> Int -> a
emissionProb emissions residue = emissions ! residue

transScore :: HMM -> HMMState -> HMMState -> Int -> Score
transScore hmm from to nodenum =
  case logProbability $ edge from to (transitions (hmm ! nodenum)) of
    NonZero p -> Score p
    LogZero -> negLogZero
 where
        -- @ start edge.tex -8
        edge :: HMMState -> HMMState -> (TProbs -> TProb)
        -- @ end edge.tex
        edge Mat Mat = m_m
        edge Mat Ins = m_i
        edge Mat Del = m_d
        edge Ins Mat = i_m
        edge Ins Ins = i_i
        edge Del Mat = d_m
        edge Del Del = d_d
        edge Beg Mat = b_m
        edge Mat End = m_e
        edge f   t   = error $ "HMM edge " ++ show f ++ " -> " ++ show t ++
                               "is allowed in the Plan7 architecture"


-- possible speedup: avoid this case analysis; substitute for maxProb in HmmPlus
transProb :: HMM -> Int -> StateAcc -> Double
transProb hmm nodenum state = case logProbability $ state (transitions (hmm ! nodenum)) of
                                   NonZero p -> p
                                   LogZero -> maxProb

myminimum :: Ord a => [Scored a] -> Scored a
myminimum [] = error "mininum of empty list"
myminimum (s:ss) = minimum' s ss
  where minimum' min [] = min
        minimum' min (s:ss) = minimum' min' ss
          where min' = if s < min then s else min
