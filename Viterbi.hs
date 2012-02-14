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
mat = 0 :: HMMState
ins = 1 :: HMMState
del = 2 :: HMMState
beg = 3 :: HMMState
end = 4 :: HMMState
bmat = 5 :: HMMState


type ScorePathCons a = a -> [a] -> [a]

consPath :: ScorePathCons a
consPath x xs = x:xs

consNoPath :: ScorePathCons a
consNoPath _ _ = []

-- hasStart and hasEnd are (for now) for model-relative local alignment.
-- when we want to consider sequence-relative local alignment, we
-- will also need to consider better of seqLocal vs. modLocal
viterbi :: ScorePathCons HMMState -> (Bool, Bool) -> Alphabet -> QuerySequence -> HMM -> (Score, StatePath)
viterbi pathCons (hasStart, hasEnd) alpha query hmm =
  if numNodes == 0 then
    (0.0, [])
  else
    flipSnd $ DL.minimum $
    [viterbi' mat (numNodes - 1) (seqlen - 1),
     viterbi' ins (numNodes - 1) (seqlen - 1),
     viterbi' del (numNodes - 1) (seqlen - 1)
    ] DL.++ if hasEnd then [bestEnd] else []


  -- trace (show state DL.++ " " DL.++ show node DL.++ " " DL.++ show obs) $
  where viterbi' state node obs = Memo.memo3 (Memo.arrayRange (mat, end)) 
                                  (Memo.arrayRange (0, numNodes))
                                  (Memo.arrayRange (0, seqlen)) 
                                  viterbi'' state node obs

        bestEnd = viterbi' end (numNodes - 1) (seqlen - 1)

        -- we see observation obs with node at state
        flipSnd pair = (fst pair, DL.reverse $ snd pair)

        numNodes = Data.Vector.length $ hmm
        seqlen = Data.Vector.length query

        res o = query ! o

        viterbi'' s 1 0 -- node 1 and zeroth observation
          | s == mat = (transProb hmm 0 m_m +
                        (emissionProb (matchEmissions $ hmm ! 1) (res 0)),
                        [mat]
                        ) -- we came from 'begin'
          | s == ins = (maxProb,[]) -- not allowed
          | s == del = (maxProb,[]) -- not allowed
        viterbi'' s 0 0 -- node 0 and zeroth observation, base of self-insert
          | s == mat = (maxProb, []) -- not allowed
          | s == ins = (transProb hmm 0 m_i +
                        emissionProb (insertionEmissions $ hmm ! 0) (res 0),
                        [ins]
                        ) -- base of insert cycle
          | s == del = (maxProb, []) -- not allowed
        viterbi'' s 0 (-1) -- node 0 and no observations
          | s == mat = (transProb hmm 0 m_m, [])
          | s == ins = (maxProb, [])
          | s == del = (maxProb, [])
        viterbi'' s 0 o -- node 0 but not zeroth observation
          | s == mat = (maxProb,[]) -- not allowed
          | s == ins = (transProb hmm 0 i_i +
                        emissionProb (insertionEmissions $ hmm ! 0) (res o) +
                        score,
                        pathCons ins path
                        ) -- possible self-insert cycle
          | s == del = (maxProb,[]) -- not allowed
          | s == end = (transProb hmm 0 m_e, [mat])
              where (score, path) = viterbi' ins 0 (o - 1)

        viterbi'' s 1 (-1) -- node 1 and no more observations (came from begin)
          | s == mat = (maxProb, []) -- not allowed
          | s == ins = (maxProb, []) -- not allowed
          | s == del = (transProb hmm 0 m_d,
                        [del]) -- came from begin
        viterbi'' s n (-1) -- not node 1 yet but no more observations (came from delete)
          | s == mat = (maxProb, []) -- not allowed
          | s == ins = (maxProb, []) -- not allowed
          | s == del = (transProb hmm (n-1) d_d +
                         score,
                         pathCons del path) -- came from delete
                          where (score, path) = viterbi' del (n - 1) (-1)
        viterbi'' s n o
          -- consume an observation AND a node
          -- I think only this equation will change when
          -- we incorporate the begin-to-match code
          | s == mat = let transition trans prevstate = 
                           if hasStart && prevstate == beg
                               then 
                                   (transProb hmm (n - 1) trans + eProb,
                                   [mat]
                                   )
                               else (score + transProb hmm (n-1) trans +
                                    eProb,
                                    mat:path
                                    )
                           where (score, path) = viterbi' prevstate (n - 1) (o - 1)
                                 eProb = emissionProb (matchEmissions $ hmm ! n) (res o)

                                     
                          in DL.minimum $ [
                                transition m_m mat, -- match came from match
                                transition i_m ins, -- match came from insert
                                transition d_m del -- match came from delete
                                ] DL.++ (if hasStart then [transition b_m beg] else [])
          -- match came from start                                            
          -- consume an observation but not a node
          | s == ins = let transition trans prevstate =
                             (score + transProb hmm n trans +
                             emissionProb (insertionEmissions $ hmm ! n) (res o),
                             ins:path
                             )
                             where (score, path) = viterbi' prevstate n (o - 1)
                        in DL.minimum [
                                transition m_i mat,   -- insert came from match
                                transition i_i ins       -- insert came from insert
                                  ]                                
          -- consume a node but not an observation
          | s == del = let transition trans prevstate =
                             (score + transProb hmm (n-1) trans,
                             del:path
                             )
                             where (score, path) = viterbi' prevstate (n - 1) o
                        in DL.minimum [
                              transition m_d mat, -- delete came from match
                              transition d_d del  -- delete came from delete
                                ]
          | s == end = let transition trans prevstate =
                             if prevstate == mat
                                 then
                                     (score + transProb hmm (n-1) trans,
                                     end:path
                                     )
                                 else
                                     (score, end:path)
                             where (score, path) = viterbi' prevstate (n-1) o
                             -- for local to QUERY we would do n, o-1.
                        in DL.minimum (if n >= 2 then [
                              transition m_e mat,
                              transition m_e end
                                ] else [transition m_e mat])

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
