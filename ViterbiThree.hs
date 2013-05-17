{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module ViterbiThree
  ( hoViterbi
  , scoreOnly
  , listViterbi
  )
where

import Data.Array as A
import qualified Data.List as L
import qualified Data.MemoCombinators as Memo
import Data.Vector.Unboxed hiding (minimum, (++), map, head, tail, null, length, foldl')
  -- XXX why import unqualified??? ---NR
import qualified Data.Vector.Unboxed as U

import qualified Constants as C
import MRFTypes hiding ( HMM
                       , m_m, m_i, m_d, i_m, i_i, d_m, d_d, b_m
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
strictScoreOnly = hoViterbi id (\(!s) _ (!s') -> s + s') xminimum
  where xminimum = L.foldl' min negLogZero



newtype NodeCount    = NC Int
  deriving (Enum, Ord, Eq, Num, Ix)
newtype ResidueCount = RC Int
  deriving (Enum, Ord, Eq, Num, Ix)


{-# INLINE hoViterbi #-}
-- | Higher-order implementation of the Viterbi algorithm, which can
-- be specialized to produce various outputs. 
-- NR's attempt to speed up Viterbi relative to C++ by reducing the amount
-- of case analysis without leaving the essentially index-based nature
-- of the computation...
-- TODO: see if this code is still manifestly isomorphic to the equations 
-- from the textbook
hoViterbi, hov1, hov2 :: (Score -> a) -- ^ reaction to initial transition
          -> (Score -> StateLabel -> a -> b) -- ^ one possible child
          -> ([b] -> a) -- ^ make answer from all children
          -> Model -> QuerySequence -> a
hov1 leaf child internal = viterbi 
 where viterbi model rs = vee' Mat (NC nMids) (RC $ U.length rs)
-- READ ME FIRST: the index types *count* items, so if we get to index 0,
-- there is nothing there...  And if we 
        where
          Model { begin = bnode, middle = middle, end = enode, midSize = nMids } = model

          x `rat` (RC i) = x U.! (pred i)
          nodeAt (NC j) = middle (NI (pred j))

          vee' = error "might as well cut my own throat"
          {-
          vee' stateRight (NC 0) rc     = beginTo stateRight rc
          vee' stateRight nc     (RC 0) = deleteAll stateRight ni
          -- @ start hoviterbi.tex -10
          vee' stateRight j i =
            internal [ child score state (vee'' state pj pi)
                     | state <- preceders stateRight
                     , let score = transition node state stateRight
                                   + emission node state aa
                     ]
            where pi = prevres  stateRight i
                  pj = prevnode stateRight j
                  node = middle pj
                  aa = rs `rat` i
          -- @ end hoviterbi.tex

          prevres Del ri = ri
          prevres _   ri = pred ri

          prevnode Ins ni = ni
          prevnode _   ni = pred ni

          beginTo Mat (RC 0) = leaf (logp (b_m bnode))
          beginTo Del (RC 0) = leaf (logp (b_d bnode))
          beginTo Mat ri     = internal [ child (logp (b_i_m bnode)) Ins (insert ri)]
            where insert (RI 0) = leaf (logp (b_i bnode))
                  insert ri     = internal [ child score Ins (insert (pred ri)) ]
                    where aa = rs `rat` ri
                          score = bitransition bnode stateRight + ((binse bnode) C./!/ aa)
          beginTo _   _      = negLogZero

       
                           
          beginTo state = leaf (beginMatch bnode state)

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
          -}


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
      Del -> 0
      -- _   -> error ("State " ++ (show state) ++ " cannot emit") 

logp = logProbability

preceders :: StateLabel -> [StateLabel]
preceders Mat = [Mat, Ins, Del]
preceders Ins = [Mat, Ins]
preceders Del = [Mat, Del]




listViterbi :: (Score -> a) -- ^ reaction to initial transition
            -> (Score -> StateLabel -> a -> b) -- ^ one possible child
            -> ([b] -> a) -- ^ make answer from all children
            -> ListModel -> [C.AA] -> a
listViterbi leaf child internal model rs = vee' Mat middles rs
 where ListModel { lbegin = bnode, lmiddle = middles } = model
       vee' stateRight [] aas = beginTo stateRight aas
       -- @ start hoviterbi.tex -10
       vee' stateRight nodes@(node:ntail) aas =
         internal [ child score state (vee'' state (nextNodes stateRight)
                                                   (nextAAs   stateRight))
                  | state <- preceders stateRight
                  , hasAA stateRight aas
                  , let score = transition node state stateRight
                                + emission node state (head aas)
                  ]
         where -- Del does not consume a residue
               -- Ins does not consume a node
               nextNodes Ins = nodes
               nextNodes _   = ntail
               nextAAs Del   = aas
               nextAAs _     = tail aas
               hasAA Del _   = True
               hasAA _  aas  = not (null aas)
       -- @ end hoviterbi.tex

       beginTo Ins _     = internal [] -- no path
       beginTo Del []    = leaf (logp (b_d bnode))
       beginTo Del (_:_) = internal [] -- no path
       beginTo Mat []    = leaf (logp (b_m bnode))
       beginTo Mat aas   = internal [child score Ins (insertAll aas)]
         where score = logp (b_i_m bnode)
               insertAll [] = leaf (logp (b_i bnode))
               insertAll (aa:aas) = internal [child score Ins (insertAll aas)]
                 where score = logp (b_i_i bnode) + (binse bnode C./!/ aa)

       memo_middle = error "memoizing nodes looks like lots more work than I thought"

       vee'' = Memo.memo3 (Memo.arrayRange (minBound, maxBound))
                          (Memo.boundedList (length middles) memo_middle)
                          (Memo.boundedList (length rs) Memo.integral)
                        vee'

hov2 leaf edge internal model rs = vee' Mat (NC nMids) (RC $ U.length rs)
 where Model { begin = bnode, middle = middle', midSize = nMids } = model
       middle  (NC i) = middle' (NI (pred i))
       residue (RC i) = rs U.! (pred i)

       vee' stateRight (NC 0) aas = beginTo stateRight aas
       -- @ start hoviterbi.tex -10
       vee' stateRight j i =
         internal [ edge score state (vee'' state (prevnode stateRight)
                                                   (prevres  stateRight))
                  | state <- preceders stateRight
                  , hasAA stateRight
                  , let score = transition node state stateRight
                                + emission node state aa
                  ]
         where -- Del does not consume a residue
               -- Ins does not consume a node
               prevnode  Ins = j
               prevnode  _   = pred j
               prevres   Del = i
               prevres   _   = pred i
               hasAA Del = True
               hasAA _   = i > 0
               aa = residue i
               node = middle j
       -- @ end hoviterbi.tex


       beginTo Ins _      = internal [] -- no path
       beginTo Del (RC 0) = leaf (logp (b_d bnode))
       beginTo Del _      = internal [] -- no path
       beginTo Mat (RC 0) = leaf (logp (b_m bnode))
       beginTo Mat rc     = internal [edge score Ins (insertAll rc)]
         where score = logp (b_i_m bnode)
               insertAll (RC 0) = leaf (logp (b_i bnode))
               insertAll rc = internal [edge score Ins (insertAll (pred rc))]
                 where score = logp (b_i_i bnode) + (binse bnode C./!/ aa)
                       aa = residue rc

       vee'' = Memo.memo3 (Memo.arrayRange (minBound, maxBound))
                          (Memo.arrayRange (0, NC (pred nMids)))
                          (Memo.arrayRange (0, RC (pred $ U.length rs)))
               vee'

hoViterbi = hov2
