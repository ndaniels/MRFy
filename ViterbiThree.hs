{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module ViterbiThree
  ( hoViterbi
  , scoreOnly
  )
where

import Data.Array as A
import qualified Data.MemoCombinators as Memo
import Data.Vector.Unboxed as U hiding (minimum, (++), map)

import qualified Constants as C
import MRFTypes hiding ( HMM
                       , m_m, m_i, m_d, i_m, i_i, d_m, d_d
                       )
import Model
import Score

type StatePath = [ StateLabel ]

type QuerySequence = U.Vector C.AA

newtype ResidueIndex = RI Int deriving (Enum, Eq, Ix, Ord)

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

scoreOnly, lazyScoreOnly, strictScoreOnly :: Model -> QuerySequence -> Score
scoreOnly = strictScoreOnly 
lazyScoreOnly   = hoViterbi id (\s _ s' -> s + s') minimum
strictScoreOnly = hoViterbi id (\(!s) _ (!s') -> s + s') minimum



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
          -> Model -> QuerySequence -> a
hoViterbi leaf child internal = viterbi 
 where viterbi model rs = vee' Mat (NI nMids) (RI $ U.length rs)
        where
          Model { begin = bnode, middle = middle, end = enode, midSize = nMids } = model

          vee' stateRight (NI 0) (RI 0)  = leaf (beginMatch bnode stateRight)
          vee' stateRight (NI 0)  ri     = insertAll stateRight ri
          vee' stateRight ni     (RI 0)  = deleteAll stateRight ni
          -- @ start hoviterbi.tex -10
          vee' stateRight j i =
            internal [ child score state (next state j i)
                     | state <- preceders stateRight -- memoized!
                     , let score = transition node state stateRight
                                   + emission node state aa
                     ]
            where node = middle (pred j)
                  aa = rs `rat` pred i
          -- @ end hoviterbi.tex

          next s@Mat ni ri = vee'' s (pred ni) (pred ri)
          next s@Del ni ri = vee'' s (pred ni) ri
          next s@Ins ni ri = vee'' s ni        (pred ri)

          insertAll stateRight (RI 0) = vee' stateRight (NI 0) (RI 0)
          insertAll stateRight ri = 
            internal [ child score stateRight (insertAll Ins (pred ri)) ]
            where aa = rs `rat` pred ri
                  score = bitransition bnode stateRight + ((binse bnode) C./!/ aa)
          -- deleteAll Ins _ = error "this can't happen" -- XXX wrong, needs inf cost 
          deleteAll Ins _ = leaf negLogZero
          deleteAll stateRight (NI 0) = vee' stateRight (NI 0) (RI 0)
          deleteAll stateRight ni =
            internal [ child score Del (deleteAll Del (pred ni)) ]
              where node = middle (pred ni)
                    score = transition node Del stateRight

          vee'' = Memo.memo3 (Memo.arrayRange (minBound, maxBound))
                           (Memo.arrayRange (NI 0, NI $ pred $ nMids))
                           (Memo.arrayRange (RI 0, RI $ pred $ U.length rs))
                           vee'
            -- N.B. could go with unsafe ranges here

beginMatch :: BeginNode -> StateLabel -> Score
beginMatch b Mat = logp (b_m_m b)
beginMatch b Ins = logp (b_m_i b)
beginMatch b Del = logp (b_m_d b)

x `rat` (RI i) = x U.! i

-- | Cost of transitions from the Ins state in the initial node
-- (which also contains the Beg state)
bitransition :: BeginNode -> StateLabel -> Score
bitransition bnode Mat = logp $ b_i_m bnode
bitransition bnode Ins = logp $ b_i_i bnode
bitransition _     Del = negLogZero

bemission :: BeginNode -> StateLabel -> C.AA -> Score
bemission bn state residue =
    case state of
      Ins -> (binse bn) C./!/ residue
      _   -> error ("State " ++ (show state) ++ "cannot emit")

transition :: MiddleNode -> StateLabel -> StateLabel -> Score
transition n from to = logp $ (edge from to) n
  where edge Mat Mat = m_m
        edge Mat Ins = m_i
        edge Mat Del = m_d
        edge Ins Mat = i_m
        edge Ins Ins = i_i
        edge Del Mat = d_m
        edge Del Del = d_d
        edge Del Ins = error "d_i impossible"
        edge Ins Del = error "i_d impossible"

emission :: MiddleNode -> StateLabel -> C.AA -> Score
emission n state residue =
    case state of
      Mat -> (mate n) C./!/ residue 
      Ins -> (inse n) C./!/ residue 
      Del -> negLogOne
      -- _   -> error ("State " ++ (show state) ++ " cannot emit") 

logp = logProbability

preceders :: StateLabel -> [StateLabel]
preceders Mat = [Mat, Ins, Del]
preceders Ins = [Mat, Ins]
preceders Del = [Mat, Del]

