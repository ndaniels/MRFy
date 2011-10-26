module HmmAlign where

import Data.Array
import Data.List

import HmmPlus

type QuerySequence = Array Int Char
type Score = Double

-- Remember, for states, 0 is a match, 1 is insertion and 2 is deletion.
-- It must be this way because the Pads parser does not support non-base
-- types as the parameter for an algebraic parser type. *sigh*
mat = 0 :: HMMState
ins = 1 :: HMMState
del = 2 :: HMMState

hmmAlign :: QuerySequence -> HMM -> Score
hmmAlign seq hmm = maximum [ hmmAlign' seq hmm seqlen mat numNodes
                           , hmmAlign' seq hmm seqlen ins numNodes
                           , hmmAlign' seq hmm seqlen del numNodes
                           ]
  where (_, seqlen) = bounds seq
        numNodes = length $ nodes hmm

hmmAlign' :: QuerySequence -> HMM -> Int -> HMMState -> Int -> Score
hmmAlign' seq hmm 0 state node
  | state == mat = 0
  | state == ins = emissionProb (insertZeroEmissions hmm)
                                (hmmAlphabet hmm)
                                (seq ! 0)
                   +
                   transProb (stateZeroTransitions hmm) m_i
  | state == del = transProb (stateZeroTransitions hmm) m_d
                        
hmmAlign' seq hmm obs state node
  | state == mat = 0
  | state == ins = 1
  | state == del = 2

emissionProb :: Eq b => [a] -> [b] -> b -> a
emissionProb emissions alphabet residue = 
  case elemIndex residue alphabet of
    Just i -> emissions !! i
    Nothing -> error "Residue not found in alphabet"

transProb transProbs stateTrans =
  case logProbability $ stateTrans transProbs of
    NonZero p -> p
    LogZero -> error "Cannot compute log 0"

