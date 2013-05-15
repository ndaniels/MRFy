module ViterbiThree
  ( hoViterbi
  , scoreOnly
  )
where

import Data.Array as A
import qualified Data.MemoCombinators as Memo
import qualified Data.Vector as V
import Data.Vector.Unboxed as U hiding (minimum, (++), map)
import Prelude hiding (pred)

import qualified Constants as C
import MRFTypes
-- import MRFTypes hiding ( HMM 
                       -- , m_m, m_i, m_d, i_m, i_i, d_m, d_d 
                       -- ) 
-- import Model 
import Score

type StatePath = [ StateLabel ]

type QuerySequence = U.Vector C.AA

data Tree = FromBegin Score
          | StepFrom [Scored (StateLabel, Tree)]

statePath :: Tree -> Scored StatePath
statePath (FromBegin s) = Scored [] s
statePath (StepFrom scores) = fmap ((:) state) $ (scoreOf s) /+/ next
  where next = statePath tree
        (state, tree) = unScored s
        s = minimum scores

cost :: Tree -> Score
cost (FromBegin s) = s
cost (StepFrom scores) = scoreOf s + cost tree
  where (_, tree) = unScored s
        s = minimum scores

addHead :: Score -> Tree -> Tree
addHead s (FromBegin s') = FromBegin (s + s')
addHead s (StepFrom subtrees) =
  StepFrom [Scored t (s + s') | Scored t s' <- subtrees]


-- inlinedTree :: (MVector NIndex nodes, MVector RIndex residues) 
            -- => BeginNode -> nodes NIndex HMMNode -> residues RIndex AA -> Tree 
-- inlinedTree = hoViterbi FromBegin (\s l t -> Scored (l, t) s) StepFrom 
--  
-- scoreOnly :: (MVector NIndex nodes, MVector RIndex residues) 
            -- => BeginNode -> nodes NIndex HMMNode -> residues RIndex AA -> Score 
-- scoreOnly = hoViterbi id (\s _ s' -> s + s') minimum 
--  
-- scoredPath :: (MVector NIndex nodes, MVector RIndex residues) 
            -- => BeginNode -> nodes NIndex HMMNode -> residues RIndex AA 
            -- -> Scored [StateLabel] 
-- scoredPath = hoViterbi (Scored []) (\s state path -> s /+/ fmap (state:) path) minimum 

scoreOnly :: HMM -> QuerySequence -> Score
scoreOnly = hoViterbi id (\s _ s' -> s + s') minimum

{-# INLINE hoViterbi #-}
-- | Higher-order implementation of the Viterbi algorithm, which can
-- be specialized to produce various outputs. 
-- NR's attempt to speed up Viterbi relative to C++ by reducing the amount
-- of case analysis without leaving the essentially index-based nature
-- of the computation...
-- TODO: see if this code is still manifestly isomorphic to the equations 
-- from the textbook
hoViterbi :: (Score -> a) -- ^ reaction to initial transition
          -> (Score -> StateLabel -> a -> b) -- ^ one possible child
          -> ([b] -> a) -- ^ make answer from all children
          -> HMM -> QuerySequence -> a
hoViterbi leaf child internal = viterbi 
 where viterbi hmm rs = ct Mat (V.length mod) (U.length rs)
        where
          bnode = hmm V.! 0
          mod = V.slice 1 (V.length hmm - 1) hmm

          ct stateRight 0  0  = leaf (transition bnode Mat stateRight)
          ct stateRight 0  ri = insertAll stateRight ri
          ct stateRight ni 0  = deleteAll stateRight ni
          ct stateRight ni ri =
            internal [ child score state (next state ni ri)
                     | state <- preceders A.! stateRight -- memoized!
                     , let score = transition node state stateRight
                                   + emission node state aa
                     ]
            where node = mod V.! pred ni
                  aa = rs U.! pred ri

          next s@Mat ni ri = ct' s (pred ni) (pred ri)
          next s@Del ni ri = ct' s (pred ni) ri
          next s@Ins ni ri = ct' s ni        (pred ri)

          insertAll stateRight 0 = ct stateRight 0 0
          insertAll stateRight ri = 
            internal [ child score stateRight (insertAll Ins (pred ri)) ]
            where aa = rs U.! pred ri
                  score = transition bnode Ins stateRight
                          + emission bnode Ins aa
          deleteAll Ins _ = leaf negLogZero
          deleteAll stateRight 0 = ct stateRight 0 0
          deleteAll stateRight ni =
            internal [ child score Del (deleteAll Del (pred ni)) ]
              where node = mod V.! pred ni
                    score = transition node Del stateRight

          ct' = Memo.memo3 (Memo.arrayRange (minBound, maxBound))
                           (Memo.arrayRange (0, pred $ V.length mod))
                           (Memo.arrayRange (0, pred $ U.length rs))
                           ct
            -- N.B. could go with unsafe ranges here

pred :: Int -> Int
pred x = x - 1

transition :: HMMNode -> StateLabel -> StateLabel -> Score
transition n Ins Del = negLogZero -- for the begin node
transition n from to = logp $ (edge from to) $ transitions n
  where edge Mat Mat = m_m
        edge Mat Ins = m_i
        edge Mat Del = m_d
        edge Ins Mat = i_m
        edge Ins Ins = i_i
        edge Del Mat = d_m
        edge Del Del = d_d
        edge Del Ins = error "d_i impossible"
        edge Ins Del = error "i_d impossible"

emission :: HMMNode -> StateLabel -> C.AA -> Score
emission n state residue =
    case state of
      Mat -> (matEmissions n) C./!/ residue 
      Ins -> (insEmissions n) C./!/ residue 
      Del -> negLogOne
      -- _   -> error ("State " ++ (show state) ++ " cannot emit") 

logp = logProbability

preceders :: A.Array StateLabel [StateLabel]
preceders = A.array (minb, maxb)
            [ (follows, [ s | s <- labels, canFollow s follows])
            | follows <- labels ]
  where labels = [minb .. maxb]
        (minb, maxb) = (Mat, Del)


-- | @canFollow s s'@ tells whether one state can follow another 
-- in the Plan7 scheme.  This predicate is symmetric:
-- order doesn't matter.
canFollow :: StateLabel -> StateLabel -> Bool
canFollow Del Ins = False
canFollow Ins Del = False
canFollow _   _   = True

