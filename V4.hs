{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module V4
  ( hoViterbi
  , scoreOnly, scoredPath, costTree, statePath, cost, Tree(..), treeDot
  , StateLabel(..)
  )
where

import Data.Array as A
import qualified Data.List as L
import qualified Data.MemoCombinators as Memo
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

import qualified Constants as C
import qualified Dot
import Model3
import Score

-- import Debug.Trace (trace)

type StatePath = [ StateLabel ]

type QuerySequence = U.Vector C.AA

newtype ResidueIndex = RI Int deriving (Enum, Eq, Ix, Ord)

data StateLabel = Mat | Ins | Del
                deriving (Show, Ord, Eq, Enum, Ix, Bounded)
data Tree = FromBegin Score
          | StepFrom [Scored (StateLabel, Tree)]

instance Show Tree where
  show (FromBegin s) = printf "Beg@%.2f" (unScore s)
  show (StepFrom []) = "Blocked"
  show (StepFrom ss) = printf "Min [%s]" $ L.intercalate ", " $ map edge ss
    where edge (Scored (state, t) score) =
            printf "%s@%.2f -> %s" (show state) (unScore score) (show t)
 

treeDot :: Tree -> String
treeDot t = Dot.toDot $ do { n <- node "E"; build n t }
  where
    node = Dot.node
    build root (FromBegin s) =
      do { b <- node "B"; Dot.edge root b (printf "%.2f" (unScore s)) }
    build _ (StepFrom []) = return ()
    build root (StepFrom ss) = mapM_ edge ss
      where edge (Scored (state, t) score) =
              do { n <- node (show state)
                 ; Dot.edge root n (printf "%.2f" (unScore score))
                 ; build n t
                 }
    

statePath :: Tree -> Scored StatePath
statePath = fmap reverse . rightToLeft
  where rightToLeft (FromBegin s) = Scored [] s
        rightToLeft (StepFrom []) = Scored [] negLogZero
        rightToLeft (StepFrom scores) = fmap ((:) state) next
          where (next, state) = minimum $ map deepScored scores
                deepScored s = (scoreOf s /+/ rightToLeft tree, state)
                  where (state, tree) = unScored s

cost :: Tree -> Score
cost (FromBegin s) = s
cost (StepFrom []) = negLogZero
cost (StepFrom scores) = scoreOf s + cost tree
  where (_, tree) = unScored s
        s = minimum scores


costTree :: Model -> QuerySequence -> Tree
costTree = hoViterbi FromBegin (\s l t -> Scored (l, t) s) StepFrom 

scoredPath :: Model -> QuerySequence -> Scored [StateLabel] 
scoredPath mod qs = fmap reverse $ hoViterbi (Scored []) combine xminimum mod qs
  where combine s state path = s /+/ fmap (state:) path
        xminimum = L.foldl' min (Scored [] negLogZero)

scoreOnly, _lazyScoreOnly, strictScoreOnly :: Model -> QuerySequence -> Score
scoreOnly = strictScoreOnly 
_lazyScoreOnly  = hoViterbi id (\s _ s' -> s + s') minimum
strictScoreOnly = hoViterbi id (\(!s) _ (!s') -> s + s') xminimum
  where xminimum = L.foldl' min negLogZero



newtype NodeCount    = NC Int
  deriving (Enum, Ord, Eq, Num, Ix)
newtype ResidueCount = RC Int
  deriving (Enum, Ord, Eq, Num, Ix)


{-# INLINE hoViterbi #-}
-- | Higher-order implementation of the Viterbi algorithm, which can
-- be specialized to produce various outputs. 

transition :: Node -> StateLabel -> StateLabel -> Score
transition n from to = edge from to n
  where edge Mat Mat = m_m . m
        edge Mat Ins = m_i . m
        edge Mat Del = m_d . m
        edge Ins Mat = i_m . i
        edge Ins Ins = i_i . i
        edge Del Mat = d_m . d
        edge Del Del = d_d . d
        edge Del Ins = error "d_i impossible"
        edge Ins Del = error "i_d impossible"

emission :: Node -> StateLabel -> C.AA -> Score
emission n state residue =
    case state of
      Mat -> (m_emission $ m n) C./!/ residue 
      Ins -> (i_emission $ i n) C./!/ residue 
      Del -> 0
      -- _   -> error ("State " ++ (show state) ++ " cannot emit") 

preceders :: StateLabel -> [StateLabel]
preceders Mat = [Mat, Ins, Del]
preceders Ins = [Mat, Ins]
preceders Del = [Mat, Del]


hoViterbi :: forall a b .
             (Score -> a) -- ^ reaction to initial transition
          -> (Score -> StateLabel -> a -> b) -- ^ one possible child
          -> ([b] -> a) -- ^ make answer from all children
          -> Model -> QuerySequence -> a
hoViterbi leaf edge internal model rs = vee' Mat (NC $ count model) (RC $ U.length rs)
 where node    (NC j) = get model (NI j)
       residue (RC i) = rs U.! i

       vee' :: StateLabel -> NodeCount -> ResidueCount -> a
       -- ^ @vee' sHat j i@ returns the min-cost path
       -- from state @Mat@ node 0 to state @sHat@ node @j@,
       -- producing the first @i@ residues from the vector @rs@.
       -- (For diagram see https://www.evernote.com/shard/s276/sh/39e47600-3354-4e8e-89f8-5c89884f9245/8880bd2c2a94dffb9be1432f12471ff2)
       -- @ start hov4.tex -7
       vee' Mat (NC 0) (RC 0) = error "reached unreachable state Mat/0/0"
       vee' Ins (NC 0) i = intoInsZero i
       vee' Mat (NC 1) i = intoMatOne i
       vee' Del (NC 1) i = intoDelOne i
       vee' stateRight j i = prevs (preceders stateRight) stateRight
                                   (predUnless j Ins) (predUnless i Del)
       -- @ end hov4.tex
               -- Ins does not consume a node; Del does not consume a residue

       {-# INLINE prevs #-}
       prevs :: [StateLabel] -- ^ potential labels @s@
             -> StateLabel   -- ^ label of the state @sHat@
             -> (StateLabel -> NodeCount) -- ^ maps @sHat@ to index of node
                                          --   containing state @s@
             -> (StateLabel -> ResidueCount) -- ^ maps @s@ to index of
                                             --   residue emitted by @s@
             -> a
       -- @prevs ss sHat fj fi@ returns the min-cost path
       -- from state @Mat@ node 0 to state @sHat@ node @j@,
       -- producing the first @i@ residues from the vector @rs@,
       -- where the path is restricted so that the state immediately
       -- before @sHat@ lies in set @ss@.
       -- Index @i@ is encoded as function @fi@, which given
       -- a candidate preceding state label @s@, produces the
       -- residue (if any) emitted by the state labelled @s@.
       -- Index @j@ is encoded as function @fj@, which given
       -- the state label @sHat@, produces the index of the node
       -- in which the preceding state is located (which
       -- is always j or @pred j@).

       -- NR: the following line is a redundant case, but it seems
       -- to every-so-slightly make things go faster.
       -- prevs []        _          _  _  = internal [] 
       prevs preceders stateRight fj fi =
        internal [ edge score state (vee'' state pj pi)
                 | state <- preceders
                 , let pi = fi state
                 , pj >= 0, pi >= 0
                 , let score = transition (node pj) state stateRight
                               + emission (node pj) state (residue pi)
                 ]
        where pj = fj stateRight

       predUnless :: forall a . Enum a => a -> StateLabel -> StateLabel -> a
       predUnless n don't_move s = if s == don't_move then n else pred n

       -- handles special non-emitting transitions into Ins state 0 and Mat state 1
       -- as well as self-transition for Ins state 0
       intoInsZero (RC 0) = leaf (transition (node 0) Mat Ins)
       intoInsZero i = prevs [Ins] Ins (\_ -> 0) (\_ -> pred i)

       intoDelOne (RC 0) = leaf (transition (node 0) Mat Del)
       intoDelOne _      = internal []

       intoMatOne (RC 0) = leaf (transition (node 0) Mat Mat)
       intoMatOne i = prevs [Ins, Del] Mat (\_ -> 0) (predUnless i Del)


       vee'' = Memo.memo3 (Memo.arrayRange (minBound, maxBound))
                          (Memo.arrayRange (0, NC (count model - 1)))
                          (Memo.arrayRange (0, RC (U.length rs - 1)))
               vee'
