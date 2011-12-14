{-# LANGUAGE BangPatterns #-}
module Viterbi where

import Debug.Trace (trace)
import Data.Char
import qualified Data.MemoCombinators as Memo
import qualified Data.List as DL

import HmmPlus
import Data.Vector
import Constants

type QuerySequence = String
type Score = Double
type StatePath = [ HMMState ]
type StateAcc = TransitionProbabilities -> TransitionProbability

-- Remember, for states, 0 is a match, 1 is insertion and 2 is deletion.
-- It must be this way because the Pads parser does not support non-base
-- types as the parameter for an algebraic parser type. *sigh*
mat = 0 :: HMMState
ins = 1 :: HMMState
del = 2 :: HMMState

-- Example output:
-- 
-- ttsmydgty-------
--         |       
-- --------LGPAEWLG 
--
-- The top sequence is the "model" or HMM sequence. All amino acids in the
-- top sequence are found by finding the most probable emission for the MATCH
-- state of that node. (Note that log probabilities are used, so we really need
-- to find the minimum.)
--
-- The bottom sequence is the query sequence (the input to smurf).
--
-- The middle is filled with spaces or pipe/bar characters depending upon state.
--
-- There are three kinds of states that affect output:
--   match:    HMM seq gets model amino acid. 
--             Middle gets pipe symbol.
--             Query seq gets query amino acid.
--   insert:   HMM seq gets '-' symbol.
--             Middle gets space character.
--             Query seq gets query amino acid.
--   deletion: HMM seq gets model amino acid.
--             Middle gets space character.
--             Query seq gets '-' symbol.
showAlignment :: HMM -> QuerySequence -> StatePath -> Int -> String -> String
showAlignment hmm query path len alpha = 
  niceify $ showA query path 0 0 [] [] []
  where showA :: String -> StatePath -> Int -> HMMState 
                 -> String -> String -> String -- correspond to the three lines
                 -> (String, String, String)
        showA _ [] _ _ oh om oq = ( DL.reverse $ DL.map toLower oh
                                  , DL.reverse om
                                  , DL.reverse $ DL.map toUpper oq
                                  )
        showA (q:qs) (p:ps) i lastp oh om oq
          | p == mat = showA qs ps (i+1) p (model:oh) ('|':om) (q:oq)
          | p == ins = showA qs ps nextInd p ('-':oh) (' ':om) (q:oq)
          | p == del = showA (q:qs) ps (i+1) p (model:oh) (' ':om) ('-':oq)

          where model = alpha !! ai
                (_, ai, _) = DL.foldr maxWithInd 
                                      (0, 0, maxProb) 
                                      (matchEmissions $ hmm ! i)
                maxWithInd :: Double -> (Int, Int, Double) -> (Int, Int, Double)                   
                maxWithInd prob (ind, mi, mp) = if prob < mp then
                                                  (ind + 1, ind, prob)
                                                else
                                                  (ind + 1, mi, mp)

                -- this is only used when we're in an Insert node
                -- thus, assume the current node is an insert
                nextInd = if lastp == ins then i else i + 1

        -- Strictly for cutting up the strings and displaying them nicely.
        -- Note: Assumes each string is in the correct order and that
        --       each string is of the same length.
        niceify :: (String, String, String) -> String
        niceify (oh, om, oq) = niceify' (DL.splitAt len oh)
                                        (DL.splitAt len om)
                                        (DL.splitAt len oq)
          where niceify' (oh, []) (om, []) (oq, []) = 
                  oh DL.++ "\n" DL.++ om DL.++ "\n" DL.++ oq
                niceify' (oh, oh') (om, om') (oq, oq') =
                  oh DL.++ "\n" DL.++ om DL.++ "\n" DL.++ oq 
                  DL.++ "\n\n"
                  DL.++ niceify' (DL.splitAt len oh') 
                                 (DL.splitAt len om') 
                                 (DL.splitAt len oq')



-- note: this returns the sequence in reverse; we'll reverse it later
-- I think we have to have a fourth case in this minimum:
-- itself a minimum of viterbi' mat for every possible final-match
-- this is the local alignment, after all
-- does this have to cover all observation-node pairs!?
-- also remember the arguments for hasStart and hasEnd
-- and this extra case is only when hasEnd
viterbi :: QuerySequence -> HMM -> String -> (Bool, Bool) -> (Score, StatePath)
viterbi querystring hmm alpha hasStart hasEnd = flipSnd $ DL.minimum
  [viterbi' mat (numNodes - 1) (seqlen - 1),
   viterbi' ins (numNodes - 1) (seqlen - 1),
   viterbi' del (numNodes - 1) (seqlen - 1)
  ] ++ if hasEnd then bestEnd else []
  
      
  where viterbi' state node obs = Memo.memo3 (Memo.arrayRange (mat, del)) 
                                  (Memo.arrayRange (0, numNodes))
                                  (Memo.arrayRange (0, seqlen)) 
                                  viterbi'' state node obs
                                  
        bestEnd = [] -- ugh. mutually recurse?                          
        -- we see observation obs with node at state
        flipSnd pair = (fst pair, DL.reverse $ snd pair)
        
        query = {-# SCC "transQuery" #-} fromList (DL.map lookup querystring)
                where lookup k = DL.elemIndex k alpha
                                    
        numNodes = Data.Vector.length $ hmm
        seqlen = Data.Vector.length query
        
        res o = query ! o
        
        viterbi'' s 1 0 -- node 1 and zeroth observation
          | s == mat = (transProb hmm 0 m_m +
                        (emissionProb (matchEmissions $ hmm ! 1) (res 0)),
                        []
                        ) -- we came from 'begin'
          | s == ins = (maxProb,[]) -- not allowed
          | s == del = (maxProb,[]) -- not allowed
        viterbi'' s 0 0 -- node 0 and zeroth observation, base of self-insert
          | s == mat = (maxProb, []) -- not allowed
          | s == ins = (transProb hmm 0 m_i +
                        emissionProb (insertionEmissions $ hmm ! 0) (res 0),
                        []
                        ) -- base of insert cycle
          | s == del = (maxProb, []) -- not allowed          
        viterbi'' s 0 o -- node 0 but not zeroth observation
          | s == mat = (maxProb,[]) -- not allowed
          | s == ins = (transProb hmm 0 i_i +
                        emissionProb (insertionEmissions $ hmm ! 0) (res o) +
                        score,
                        ins:path
                        ) -- possible self-insert cycle
          | s == del = (maxProb,[]) -- not allowed
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
                         del:path) -- came from delete
                          where (score, path) = viterbi' del (n - 1) (-1)
        viterbi'' s n o
          -- consume an observation AND a node
          -- I think only this equation will change when
          -- we incorporate the begin-to-match code
          | s == mat = let transition trans prevstate = 
                               (score + transProb hmm (n-1) trans +
                                emissionProb (matchEmissions $ hmm ! n) (res o),
                                mat:path
                               )
                               where (score, path) = viterbi' prevstate (n - 1) (o - 1)
                          in DL.minimum [
                                transition m_m mat, -- match came from match
                                transition i_m ins, -- match came from insert
                                transition d_m del  -- match came from delete
                                ]
                                                                            
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
          
-- TODO preprocessing: convert hmm to array of nodes with the stateZero and insertZero stuff prepended
-- this will transform `node` below and `transProb` below
                                    
emissionProb :: [a] -> Maybe Int -> a
emissionProb emissions residue = 
  case residue of
    Just i -> emissions !! i
    Nothing -> error "Residue not found in alphabet"

transProb :: HMM -> Int -> StateAcc -> Double
transProb hmm nodenum state = case logProbability $ state (transitions (hmm ! nodenum)) of
                                   NonZero p -> p
                                   LogZero -> maxProb
