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
    [viterbi' Mat (numNodes - 1) (seqlen - 1),
     viterbi' Ins (numNodes - 1) (seqlen - 1),
     viterbi' Del (numNodes - 1) (seqlen - 1)
    ] DL.++ if hasEnd then [bestEnd] else []


  -- trace (show state DL.++ " " DL.++ show node DL.++ " " DL.++ show obs) $
  where viterbi' state j i = Memo.memo3 (Memo.arrayRange (Mat, End)) 
                                  (Memo.arrayRange (0, numNodes))
                                  (Memo.arrayRange (0, seqlen)) 
                                  viterbi'' state j i

        bestEnd = viterbi' End (numNodes - 1) (seqlen - 1)

        -- we see observation obs with node at state
        flipSnd pair = (fst pair, DL.reverse $ snd pair)

        numNodes = Data.Vector.length $ hmm
        seqlen = Data.Vector.length query

        res i = query ! i

        -- node 1 and zeroth observation
        viterbi'' Mat 1 0 = ( transProb hmm 0 m_m +
                              emissionProb (matchEmissions $ hmm ! 1) (res 0)
                            , [Mat]
                            ) -- we came from 'begin'
        viterbi'' Ins 1 0 = (maxProb, []) -- not allowed
        viterbi'' Del 1 0 = (maxProb, []) -- not allowed

        -- node 0 and zeroth observation, base of self-insert
        viterbi'' Mat 0 0 = (maxProb, []) -- not allowed
        viterbi'' Ins 0 0 = ( transProb hmm 0 m_i +
                              emissionProb (insertionEmissions $ hmm ! 0) (res 0)
                            , [Ins]
                            )
        viterbi'' Del 0 0 = (maxProb, []) -- not allowed

        -- node 0 and no observations
        viterbi'' Mat 0 (-1) = (transProb hmm 0 m_m, [])
        viterbi'' Ins 0 (-1) = (maxProb, [])
        viterbi'' Del 0 (-1) = (maxProb, [])

        -- node 0 but not zeroth observation
        viterbi'' Mat 0 i = (maxProb,[]) -- not allowed
        viterbi'' Ins 0 i = ( transProb hmm 0 i_i +
                              emissionProb (insertionEmissions $ hmm ! 0) (res i) +
                              score
                            , pathCons Ins path
                            ) -- possible self-insert cycle
          where (score, path) = viterbi' Ins 0 (i - 1)
        viterbi'' Del 0 i = (maxProb, []) -- not allowed
        viterbi'' End 0 i = (transProb hmm 0 m_e, [Mat])

        -- node 1 and no more observations (came from begin)
        viterbi'' Mat 1 (-1) = (maxProb, []) -- not allowed
        viterbi'' Ins 1 (-1) = (maxProb, []) -- not allowed
        viterbi'' Del 1 (-1) = (transProb hmm 0 m_d, [Del]) -- came from begin

        -- not node 1 yet, but not more observations (came from delete)
        viterbi'' Mat j (-1) = (maxProb, []) -- not allowed
        viterbi'' Ins j (-1) = (maxProb, []) -- not allowed
        viterbi'' Del j (-1) = ( transProb hmm (j - 1) d_d + score
                               , pathCons Del path
                               ) -- came from delete
          where (score, path) = viterbi' Del (j - 1) (-1)

        -- consume an observation AND a node
        -- I think only this equation will change when
        -- we incorporate the begin-to-match code
        viterbi'' Mat j i = DL.minimum $ [ trans m_m Mat -- match came from match
                                         , trans i_m Ins -- match came from insert
                                         , trans d_m Del -- match came from delete
                                         ]
          where trans transFn prevstate =
                  (score + tProb + eProb, pathCons Mat path)
                  where (score, path) = viterbi' prevstate (j - 1) (i - 1)
                        eProb = emissionProb (matchEmissions $ hmm ! j) (res i)
                        tProb = transProb hmm (j - 1) transFn

        -- match came from start                                            
        -- consume an observation but not a node
        viterbi'' Ins j i = DL.minimum [ transition m_i Mat, transition i_i Ins ]
          where transition trans prevstate =
                  (score + transProb hmm j trans +
                    emissionProb (insertionEmissions $ hmm ! j) (res i)
                  , pathCons Ins path
                  )
                  where (score, path) = viterbi' prevstate j (i - 1)

        -- consume a node but not an observation
        viterbi'' Del j i = DL.minimum [ transition m_d Mat, transition d_d Del ]
          where transition trans prevstate =
                  (score + transProb hmm (j - 1) trans, pathCons Del path)
                  where (score, path) = viterbi' prevstate (j - 1) i

        viterbi'' End j i = DL.minimum $
                              if j >= 2 then
                                [ transition m_e Mat, transition m_e End ]
                              else
                                [ transition m_e Mat ]
          where transition trans prevstate =
                  case prevstate of
                    Mat -> (score + transProb hmm (j - 1) trans, pathCons End path)
                    otherwise -> (score, pathCons End path)
                  where (score, path) = viterbi' prevstate (j - 1) i
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
