module HmmAlign where

import Data.Array
import Data.List
import Debug.Trace (trace)

import HmmPlus

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

-- viterbi :: QuerySequence -> HMM -> (Score, StatePath) 
-- viterbi seq hmm = maximum [ hmmAlign seq hmm (seqlen - 1) mat (numNodes - 1) 
                          -- , hmmAlign seq hmm (seqlen - 1) ins (numNodes - 1) 
                          -- , hmmAlign seq hmm (seqlen - 1) del (numNodes - 1) 
                          -- ] 
  -- where (_, seqlen) = bounds seq 
        -- numNodes = length $ nodes hmm 

viterbi_memo :: QuerySequence -> HMM -> (Score, StatePath)
viterbi_memo seq hmm = minimum [ arr ! (seqlen - 1, mat, numNodes - 1)
                               , arr ! (seqlen - 1, ins, numNodes - 1)
                               , arr ! (seqlen - 1, del, numNodes - 1)
                               ]
  where (_, seqlen) = {-# SCC "bounds" #-} bounds seq
        numNodes = length $ nodes hmm
        arr = listArray ((0, 0, 0), (seqlen - 1, 2, numNodes - 1))
                        [ {-# SCC "hmmAlign" #-} hmmAlign seq hmm o s n
                        | o <- [0..seqlen - 1]
                        , s <- [0..2]
                        , n <- [0..numNodes - 1]
                        ]

        hmmAlign :: QuerySequence -> HMM -> Int -> HMMState -> Int -> (Score, StatePath)
        hmmAlign seq hmm 0 state nodenum
          | state == mat = (0, [mat])
          | state == ins = ( emissionProb (insertZeroEmissions hmm)
                                          (hmmAlphabet hmm)
                                          (seq ! 0)
                             +
                             transProb hmm 0 m_i
                           , [ins]
                           )
          | state == del = (transProb hmm 0 m_d, [del])
        hmmAlign seq hmm obs state 0
          | state == mat = (0, [mat])
          | state == ins = let (score, path) = {-# SCC "align" #-} align 0 (obs - 1) i_i ins
                           in  (score, ins : path)
          | state == del = (0, [del])
          where align :: Int -> Int -> StateAcc -> HMMState -> (Score, StatePath)
                align nodenum obs stateFun state = 
                  -- let (score, path) = hmmAlign seq hmm obs state nodenum 
                  let (score, path) = arr ! (obs, state, nodenum)
                  in  (score + transProb hmm nodenum stateFun, path)
        hmmAlign seq hmm obs state nodenum
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
          where node = (nodes hmm) !! nodenum
                alpha = hmmAlphabet hmm
                res = seq ! obs

                align :: Int -> Int -> StateAcc -> HMMState -> (Score, StatePath)
                align nodenum obs stateFun state = {-# SCC "align2" #-}
                  -- let (score, path) = hmmAlign seq hmm obs state nodenum 
                  let (score, path) = arr ! (obs, state, nodenum)
                  in  (score + transProb hmm nodenum stateFun, path)


emissionProb :: Eq b => [a] -> [b] -> b -> a
emissionProb emissions alphabet residue = 
  case elemIndex residue alphabet of
    Just i -> emissions !! i
    Nothing -> error "Residue not found in alphabet"

-- This function may need to be changed after we filter the stuff from Pads.
-- 1) We will use arrays instead of lists
-- 2) We may put the '0' node in with the other HmmNodes
transProb :: HMM -> Int -> StateAcc -> Double
transProb hmm nodenum state = case logProbability $ state trans of
                                   NonZero p -> p
                                   LogZero -> 100000
                                   -- LogZero -> error "Cannot compute log 0" 
  where trans = if nodenum == 0 then stateZeroTransitions hmm
                                else transitions ((nodes hmm) !! nodenum)

