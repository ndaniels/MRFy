{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module V4
  ( hoViterbi
  , scoreOnly, scoredPath, costTree, statePath, cost, Tree(..), treeDot
  , scorePlusX
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

scorePlusX :: Model -> QuerySequence -> Score -> Score
scorePlusX m q s = hoViterbi (s+) (\(!s) _ (!s') -> s + s') xminimum m q
  where xminimum = L.foldl' min negLogZero


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
-- @ start v4aux.tex
preceders Mat = [Mat, Ins, Del]
preceders Ins = [Mat, Ins]
preceders Del = [Mat, Del]
-- @ end v4aux.tex

hoViterbi :: forall a edge .
             (Score -> a) -- ^ reaction to initial transition
          -> (Score -> StateLabel -> a -> edge) -- ^ one possible child
          -> ([edge] -> a) -- ^ make answer from all children
          -> Model -> QuerySequence -> a
hoViterbi lf edg int model rs = vee' Mat (NI $ count model) (RC $ U.length rs)
 where 
       {-# INLINE leaf #-}
       {-# INLINE edge #-}
       {-# INLINE internal #-}
       -- @ start hosigs.tex -7
       leaf     :: Score -> a
       edge     :: Score -> StateLabel -> a -> edge
       internal :: [edge] -> a
       -- @ end hosigs.tex -7
       leaf = lf
       edge = edg
       internal = int

       node    j      = get model j
       residue (RC i) = rs U.! i

      
       -- @ start hov4.tex -7
       vee' :: StateLabel -> NodeIndex -> ResidueCount -> a
       -- @ end hov4.tex
       -- ^ @vee' sHat j i@ returns the min-cost path
       -- from state @Mat@ node 0 to state @sHat@ node @j@,
       -- producing the first @i@ residues from the vector @rs@.
       -- (For diagram see https://www.evernote.com/shard/s276/sh/39e47600-3354-4e8e-89f8-5c89884f9245/8880bd2c2a94dffb9be1432f12471ff2)
       -- @ start hov4.tex -7
       vee' Del (NI 1) (RC 0) = leaf (transition (node 0) Mat Del)
       vee' Ins (NI 0) (RC 0) = leaf (transition (node 0) Mat Ins)
       vee' Mat (NI 1) (RC 0) = leaf (transition (node 0) Mat Mat)
       -- @ end hov4.tex
       vee' Mat j@(NI 1) i = prevs [Ins, Mat, Del] Mat 
                                   (predUnless j Ins) (predUnless i Del)

       -- @ start hov4.tex -7
       vee' stateHat j i = 
       -- @ end hov4.tex
         prevs (preceders stateHat) stateHat (predUnless j Ins) (predUnless i Del)
       _unused stateHat j i = 
       -- @ start hov4.tex -7
         internal [ edge score state (vee'' state pj pi)
                  | state <- preceders stateHat
                  , let pi = predUnless i Del state
                  , pj >= 0, pi >= 0
                  , let score = transition (node pj) state stateHat
                                + emission (node pj) state (residue pi)
                  ]
         where pj = predUnless j Ins stateHat
       -- @ end hov4.tex
       -- @ start hov-prevs.tex
       prevs predecessors stateHat fj fi =
        internal [ edge score state (vee'' state pj pi)
                 | state <- predecessors
                 , let pi = fi state
                 , pj >= 0, pi >= 0
                 , let score = transition (node pj) state stateHat
                               + emission (node pj) state (residue pi)
                 ]
        where pj = fj stateHat
       -- @ end hov-prevs.tex
               -- Ins does not consume a node; Del does not consume a residue

       {-# INLINE prevs #-}
       {-
       prevs :: [StateLabel] -- ^ potential labels @s@
             -> StateLabel   -- ^ label of the state @sHat@
             -> (StateLabel -> NodeIndex) -- ^ maps @sHat@ to index of node
                                          --   containing state @s@
             -> (StateLabel -> ResidueCount) -- ^ maps @s@ to index of
                                             --   residue emitted by @s@
             -> a
       -}
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
       -- @ start hov-prevs.tex -7
       prevs :: [StateLabel] -> StateLabel
             -> (StateLabel -> NodeIndex) -> (StateLabel -> ResidueCount) -> a
       -- @ end hov-prevs.tex -7

       -- @ start v4aux.tex -7
       predUnless :: forall a . Enum a => a -> StateLabel -> StateLabel -> a
       predUnless n don't_move s = if s == don't_move then n else pred n
       -- @ end v4aux.tex

       vee'' = Memo.memo3 (Memo.arrayRange (minBound, maxBound))
                          (Memo.arrayRange (0, NI (count model - 1)))
                          (Memo.arrayRange (0, RC (U.length rs)))
               vee'
