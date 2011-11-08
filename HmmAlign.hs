module HmmAlign where

import qualified Data.Array as A
import Data.List
import Debug.Trace (trace)
import qualified Data.Vector as V

import Constants
import HmmPlus

type QuerySequence = A.Array Int Char
type Score = Double
type StatePath = [ HMMState ]
type StateAcc = TransitionProbabilities -> TransitionProbability

-- Remember, for states, 0 is a match, 1 is insertion and 2 is deletion.
-- It must be this way because the Pads parser does not support non-base
-- types as the parameter for an algebraic parser type. *sigh*
mat = 0 :: HMMState
ins = 1 :: HMMState
del = 2 :: HMMState


viterbi_memo :: QuerySequence -> HMM -> (Score, StatePath)
viterbi_memo seq hmm = minimum [ arr A.! (seqlen - 1, mat, numNodes - 1)
                               , arr A.! (seqlen - 1, ins, numNodes - 1)
                               , arr A.! (seqlen - 1, del, numNodes - 1)
                               ]
  where (_, seqlen) = {-# SCC "bounds" #-} A.bounds seq
        numNodes = V.length hmm
        arr = A.listArray ((-1, 0, 0), (seqlen - 1, 2, numNodes - 1))
                        [ {-# SCC "hmmAlign" #-} hmmAlign o s n
                        | o <- [-1..seqlen - 1]
                        , s <- [0..2]
                        , n <- [0..numNodes - 1]
                        ]
        alpha = amino
        res = (A.!) seq

        align :: Int -> Int -> StateAcc -> HMMState -> (Score, StatePath)
        align nodenum obs stateFun state = {-# SCC "align2" #-}
          let (score, path) = arr A.! (obs, state, nodenum)
          in  (score + transProb hmm nodenum stateFun, path)

        hmmAlign :: Int -> HMMState -> Int -> (Score, StatePath)
        hmmAlign 0 state 1
          | state == mat = ( transProb hmm 0 m_m
                             + (emissionProb (matchEmissions (hmm V.! 1)) 
                                             alpha
                                             (res 0))
                           , []
                           )
          | state == ins = (maxProb, [])
          | state == del = (maxProb, [])
        hmmAlign 0 state 0
          | state == mat = (maxProb, [])
          | state == ins = ( transProb hmm 0 m_i
                             + emissionProb (matchEmissions (hmm V.! 0))
                                            alpha
                                            (res 0)
                           , []
                           )
          | state == del = (maxProb, [])
        hmmAlign obs state 0
          | state == mat = (maxProb, [])
          | state == ins = ( score 
                             + emissionProb (insertionEmissions $ hmm V.! 0)
                                            alpha
                                            (res obs)
                           , ins : path
                           )
          | state == del = (maxProb, [])
          where (score, path) = align 0 (obs - 1) i_i ins 
        hmmAlign (-1) state 1
          | state == mat = (maxProb, [])
          | state == ins = (maxProb, [])
          | state == del = (transProb hmm 0 m_d, [del])          
        hmmAlign (-1) state nodenum
          | state == mat = (maxProb, [])
          | state == ins = (maxProb, [])
          | state == del = ( transProb hmm (nodenum - 1) d_d
                             + score
                           , del : path
                           )
              where (score, path) = align (nodenum - 1) (-1) d_d del
        hmmAlign obs state nodenum
          | state == mat = 
              let (score, path) = minimum [ align (nodenum - 1) (obs - 1) m_m mat
                                          , align (nodenum - 1) (obs - 1) i_m ins
                                          , align (nodenum - 1) (obs - 1) d_m del
                                          ]
              in  (score + emissionProb (matchEmissions node) alpha res, mat : path)
          | state == ins = 
              let (score, path) = minimum [ align nodenum (obs - 1) m_i mat
                                          , align nodenum (obs - 1) i_i ins
                                          -- , (-1, [])
                                          ]
              in  (score + emissionProb (insertionEmissions node) alpha res, ins : path)
          | state == del = 
              let (score, path) = minimum [ align (nodenum - 1) obs m_d mat
                                          -- , (-1, [])
                                          , align (nodenum - 1) obs d_d del
                                          ]
              in  (score, del : path)
          where node = hmm V.! nodenum
                res = seq A.! obs


emissionProb :: Eq b => [a] -> [b] -> b -> a
emissionProb emissions alphabet residue = 
  case elemIndex residue alphabet of
    Just i -> emissions !! i
    Nothing -> error "Residue not found in alphabet"

-- This function may need to be changed after we filter the stuff from Pads.
-- 1) We will use arrays instead of lists
-- 2) We may put the '0' node in with the other HmmNodes
transProb :: HMM -> Int -> StateAcc -> Double
transProb hmm nodenum state = 
  case logProbability $ state (transitions (hmm V.! nodenum)) of
       NonZero p -> p
       LogZero -> maxProb

-- transProb :: HMM -> Int -> StateAcc -> Double 
-- transProb hmm nodenum state = case logProbability $ state trans of 
                                   -- NonZero p -> p 
                                   -- LogZero -> 100000 
                                   -- -- LogZero -> error "Cannot compute log 0"  
  -- where trans = if nodenum == 0 then stateZeroTransitions hmm 
                                -- else transitions ((nodes hmm) !! nodenum) 

