{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Viterbi
       ( QuerySequence
       , StatePath
       , viterbi
       , transScore
       , transScoreNode
       , emissionScore
       , emissionScoreNode
       , consPath
       , consNoPath
       , TProb
       , TProbs
       )
where

import Debug.Trace (trace)
-- import Debug.Trace.LocationTH (check) 
import qualified Data.MemoCombinators as Memo
import qualified Data.List as DL
import Data.Ix
import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable


import Beta
import HMMPlus
import Data.Vector.Unboxed as U hiding (minimum, (++), map)
import qualified Data.Vector as V hiding (minimum, (++), map)
import Constants
import MRFTypes
import Score

type QuerySequence = U.Vector AA

type StatePath = [ StateLabel ]

type ScorePathCons a = a -> [a] -> [a]

unscore :: Scored a -> (Score, a)
unscore (Scored a x) = (x, a)

consPath :: ScorePathCons a
consPath x xs = x:xs

consNoPath :: ScorePathCons a
consNoPath _ _ = []

-- hasStart and hasEnd are (for now) for model-relative local alignment.
-- when we want to consider sequence-relative local alignment, we
-- will also need to consider better of seqLocal vs. modLocal
viterbi :: ScorePathCons StateLabel -> (Bool, Bool) ->
           QuerySequence -> HMM -> Scored StatePath
viterbi pathCons (hasStart, hasEnd) query hmm =
  if numNodes == 0 then
    Scored [] negLogOne
  else
    minimum $ [vee'' s (numNodes - 1) (seqlen - 1) | s <- [Mat, Ins, Del]] ++ 
              if hasEnd then [bestEnd] else []

  -- trace (show state DL.++ " " DL.++ show node DL.++ " " DL.++ show obs) $
  where 
        -- @ start memo.tex -8
        vee'' = Memo.memo3 (Memo.arrayRange (Mat, End)) 
                           (Memo.arrayRange (0, numNodes))
                           (Memo.arrayRange (-1, seqlen)) 
                           vee'
        -- @ end memo.tex

        bestEnd = vee' End (numNodes - 1) (seqlen - 1)

        numNodes = V.length $ hmm
        seqlen = U.length query

        res i = query ! i
        
        extend :: StateLabel -> Scored StatePath -> Scored StatePath
        extend s = fmap (pathCons s)
        
        aScore :: StateLabel -> StateLabel -> Int -> Score
        aScore = transScore hmm
        
        eScore :: StateLabel -> Int -> Int -> Score
        eScore = emissionScore hmm query
        
        disallowed = Scored [] negLogZero -- outcome of zero likelihood
        
        vee' :: StateLabel -> Int -> Int -> Scored [StateLabel]
        
        -- 1  0 Mat
        -- 0  0 Ins
        -- 0 -1 Mat
        -- 1 -1 Del
        -- 0  i Ins
        -- 0  i End
        -- j -1 Del
        -- j  i Ins, Mat, Del, End

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
        vee' Ins 0 i = extend Ins $     -- possible self-insert cycle
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
        --------------------------------------------------------
        -- @ start viterbi.tex -8
        vee' Mat j i = extend Mat $
           eScore Mat j i /+/ minimum (map avSum [Mat, Ins, Del])
         where avSum prev =
                 aScore prev Mat (j-1) /+/ vee'' prev (j-1) (i-1)
        -- @ end viterbi.tex

        -- consume an observation but not a node
        vee' Ins j i = extend Ins
                       (eScore Ins j i /+/ minimum (map from [Mat, Ins]))
         where from prev = aScore prev Ins j /+/ vee'' prev j (i-1)

        -- consume a node but not an observation
        vee' Del j i = extend Del $ minimum (map from [Mat, Del])
          where from prev = aScore prev Del (j-1) /+/ vee'' prev (j-1) i

        vee' End j i = extend End $ minimum (map from preds)
         where preds = if j >= 2 then [Mat, End] else [Mat]
               from Mat  = aScore Mat Mat (j-1) /+/ vee'' Mat  (j-1) i
               from prev =                          vee'' prev (j-1) i
                        -- for local to QUERY we would do j, i-1.

-- TODO seqLocal: consider the case where we consume obs, not state, for beg & end.

emissionScoreNode :: HMMNode -> AA -> StateLabel -> Score
emissionScoreNode n q state = 
    case state of
      Mat -> (matchEmissions     n) /!/ q 
      Ins -> (insertionEmissions n) /!/ q 
      _   -> error ("State " ++ (show state) ++ "cannot emit")

emissionScore :: HMM -> QuerySequence -> StateLabel -> Int -> Int -> Score
emissionScore hmm qs state j i = emissionScoreNode (hmm V.! j) (qs U.! i) state

transScoreNode n from to =
  logProbability $ edge from to (transitions n)
 where
        -- @ start edge.tex -8
        edge :: StateLabel -> StateLabel -> (TProbs -> TProb)
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
                               " is not allowed in the Plan7 architecture"

transScore :: HMM -> StateLabel -> StateLabel -> Int -> Score
transScore hmm from to nodenum = transScoreNode (hmm V.! nodenum) from to

