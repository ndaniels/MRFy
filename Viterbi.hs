module Viterbi where

import qualified Data.MemoCombinators as Memo
import Data.List

import HmmPlus

import Constants

type QuerySequence = Array Int Char
type Score = Double
type StatePath = [ HMMState ]
type StateAcc = TransitionProbabilities -> TransitionProbability

-- Remember, for states, 0 is a match, 1 is insertion and 2 is deletion.
-- It must be this way because the Pads parser does not support non-base
-- types as the parameter for an algebraic parser type. *sigh*
mat = 0 :: HMMState
ins = 1 :: HMMState
del = 2 :: HMMState

-- note: this returns the sequence in reverse; we'll reverse it later
viterbi :: QuerySequence -> HMM -> (Score, StatePath)
viterbi query hmm = minimum
  [viterbi' mat (numNodes - 1) (seqlen - 1),
   viterbi' ins (numNodes - 1) (seqlen - 1),
   viterbi' del (numNodes - 1) (seqlen - 1)
  ]
  
      
  where viterbi' state node obs = Memo.memo3 Memo.integral Memo.integral Memo.char 
                                  viterbi'' state node obs
        -- we see observation obs with node at state
                                    
        numNodes = length $ nodes hmm
        seqlen = length query
        
        viterbi'' s 1 0 -- node 1 and zeroth observation
          | s == mat = (transProb hmm 0 m_m +
                        (emissionProb (matchEmissions node $ 1) alpha (res 0)),
                        []
                        ) -- we came from 'begin'
          | s == ins = (maxProb,[]) -- not allowed
          | s == del = (maxProb,[]) -- not allowed
        viterbi'' s 0 0 -- node 0 and zeroth observation, base of self-insert
          | s == mat = (maxProb, []) -- not allowed
          | s == ins = (transProb hmm 0 m_i +
                        emissionProb (matchEmissions node $ 0), []) -- base of insert cycle
          | s == del = (maxProb, []) -- not allowed          
        viterbi'' s 0 o -- node 0 but not zeroth observation
          | s == mat = (maxProb,[]) -- not allowed
          | s == ins = (transProb hmm 0 i_i +
                        emissionProb (insertZeroEmissions hmm) alpha (res o) +
                        score,
                        ins:path
                        ) -- possible self-insert cycle
          | s == del = (maxProb,[]) -- not allowed
              where (score, path) = viterbi'' ins 0 (o - 1)
        -- viterbi'' s n 0 -- not node 1 yet but zeroth observation (this case may go away). yes, roll into general case
        --   | s == mat = (transProb hmm (n-1) d_m + 
        --                 emissionProb (matchEmissions node $ 0) +
        --                 score,
        --                 mat:path)
        --   | s == ins = (maxProb, []) -- not allowed because no d-i transitions
        --   | s == del = (transProb hmm (n-1) del + -- del becomes either d_d or m_d
        --                 score,
        --                 del:path) -- came from delete
        --       where (score, path) = viterbi'' del (n - 1) (-1) -- WRONG
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
                         del:path del) -- came from delete
                          where (score, path) = viterbi'' del (n - 1) (-1)
        viterbi'' s n o
          -- consume an observation AND a node
          | s == mat = let (score, path) = viterbi'' prevstate (n - 1) (o - 1)
                       in let transition trans prevstate = 
                               (transProb hmm (n-1) trans +
                                emissionProb (matchEmissions node $ o) +
                                score,
                                mat:path
                               )
                          in minimum [
                                transition m_m mat, -- match came from match
                                transition i_m ins, -- match came from insert
                                transition d_m del  -- match came from delete
                                ]
                             
          -- | s == mat = minimum [
          --                       transition m_m mat, -- match came from match
          --                       transition i_m ins, -- match came from insert
          --                       transition d_m del  -- match came from delete
          --                       ]
          --                       where transition trans prevstate = 
          --                               (transProb hmm (n-1) trans +
          --                                emissionProb (matchEmissions node $ o) +
          --                                score,
          --                                mat:path
          --                               )
          --                               where (score, path) =
          --                                       viterbi'' prevstate (n - 1) (o - 1)                                                
          -- consume an observation but not a node
          -- how do we fix parse error? would a let-binding help?
          | s == ins = let (score s', path s') = viterbi'' s' n (o - 1)
                          in let transition trans prevstate prevnode =
                            (transProb hmm n trans +
                             emissionProb (insertEmissions node $ o) +
                             (score prevstate),
                             ins:(path prevstate)
                            )
                              in minimum [
                                      transition m_i mat,   -- insert came from match
                                      transition i_i ins       -- insert came from insert
                                      ]
          -- | s == ins = minimum [
          --                       transition m_i mat,   -- insert came from match
          --                       transition i_i ins       -- insert came from insert
          --                       ]
          --                       where transition trans prevstate prevnode =
          --                         (transProb hmm n trans +
          --                          emissionProb (insertEmissions node $ o) +
          --                          (score prevstate),
          --                          ins:(path prevstate)
          --                         )
          --                         where (score s', path s') = viterbi'' s' n (o - 1)                                  
          -- consume a node but not an observation
          | s == del = minimum [
                                transition m_d mat, -- delete came from match
                                transition d_d del  -- delete came from delete
                                ]
                                where transition trans prevstate =
                                  (transProb hmm (n-1) trans +
                                  (score prevstate),
                                  del:(path prevstate)
                                  )
                                  where (score s', path s') = viterbi'' s' (n - 1) o
          
-- TODO preprocessing: convert hmm to array of nodes with the stateZero and insertZero stuff prepended
-- this will transform `node` below and `transProb` below
          where node n' = (nodes hmm) !! n' -- optimize this!
                alpha = hmmAlphabet hmm
                res o' = seq ! o'
                                    


transProb :: HMM -> Int -> StateAcc -> Double
transProb hmm nodenum state = case logProbability $ state trans of
                                   NonZero p -> p
                                   LogZero -> maxProb
                                   -- LogZero -> error "Cannot compute log 0" 
  where trans = if nodenum == 0 then stateZeroTransitions hmm
                                else transitions ((nodes hmm) !! nodenum)    