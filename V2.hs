{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module V2
       ( Tree(..), AA(..), StateLabel(..), FullStateLabel(..)
       , HMMNode(..), BeginNode(..)
       , costTree, canFollow
       , aCostTree
       )
where

import qualified Data.MemoCombinators as Memo

import Data.Array
import Score


newtype AA = AA Int
             deriving (Eq, Ord, Ix)

data StateLabel = Mat | Ins | Del
                deriving (Eq, Ord, Enum, Show, Ix, Bounded)
data FullStateLabel = Beg | End | Other StateLabel
                deriving (Eq, Ord, Show)

data HMMNode = HMMNode { transitionsOut :: Array (StateLabel, StateLabel) Score
                       , emissions :: Array (StateLabel, AA) Score
                       }

data BeginNode = BN { beginTransitions :: Array StateLabel Score
                    , begin_i_m :: Score
                    , begin_i_i :: Score
                    , begin_emissions_i :: Array AA Score
                    }

data Tree = FromBegin Score
          | StepFrom [Scored (StateLabel, Tree)]

emission :: HMMNode -> StateLabel -> AA -> Score
emission node label aa = emissions node ! (label, aa)

class Transit node where
  type LeftLabel node
  transitionx :: node -> LeftLabel node -> StateLabel -> Score
  transition  :: node -> StateLabel -> StateLabel -> Score
  
instance Transit HMMNode where
  type LeftLabel HMMNode = StateLabel
  transition node l r = transitionsOut node ! (l, r)
  transitionx = transition
  

btransition :: BeginNode -> StateLabel -> Score
btransition node state = beginTransitions node ! state


costTree :: BeginNode -> [HMMNode] -> [AA] -> Tree
costTree bnode = ct Mat -- damn lie, leaves off the end transition
  where ct stateRight [] [] = FromBegin (btransition bnode stateRight)
        ct stateRight [] aas = insertAll stateRight aas
        ct stateRight nodes [] = deleteAll stateRight nodes
        ct stateRight nodes@(node:ntail) aas@(aa:aatail) =
          StepFrom [ Scored (state, t) score
                   | state <- preceders ! stateRight
                   , let t = next state nodes ntail aas aatail
                   , let score = transition node state stateRight +
                                 emission node state aa
                   ]
        next s@Mat _ ntail _   aatail = ct' s ntail aatail
        next s@Del _ ntail aas _      = ct' s ntail aas
        next s@Ins nodes _ _   aatail = ct' s nodes aatail

        insertAll Del _ = error "this can't happen" -- XXX wrong, needs inf cost
        insertAll stateRight [] = ct stateRight [] []
        insertAll Mat aas = begin_i_m bnode `addHead` insertAll Ins aas
        insertAll Ins (aa:aas) =
          StepFrom [Scored (Ins, insertAll Ins aas)
                    (begin_i_i bnode + begin_emissions_i bnode ! aa)]

        deleteAll Ins _ = error "this can't happen" -- XXX wrong, needs inf cost
        deleteAll stateRight [] = ct stateRight [] []
        deleteAll stateRight (node:nodes) = 
          StepFrom [Scored (Del, deleteAll Del nodes) (transition node Del stateRight)]

        ct' = ct -- memoize here; probably worth trying both Memo.list
                 -- and also Memo.wrap

-- | @canFollow s s'@ tells whether one state can follow another 
-- in the Plan7 scheme.  This predicate is symmetric:
-- order doesn't matter.
canFollow :: StateLabel -> StateLabel -> Bool
canFollow Del Ins = False
canFollow Ins Del = False
canFollow _   _   = True


addHead :: Score -> Tree -> Tree
addHead s (FromBegin s') = FromBegin (s + s')
addHead s (StepFrom subtrees) =
  StepFrom [Scored t (s + s') | Scored t s' <- subtrees]


----------- array version

class MVector index array where -- ^ 0-origin
  at :: array index a -> index -> a
  mvlength :: array index a -> Int
  -- mvelems :: array index a -> [a]
  
newtype NIndex = NI Int
               deriving (Ix, Ord, Eq, Enum)
newtype RIndex = RI Int
               deriving (Ix, Ord, Eq, Enum)

preceders :: Array StateLabel [StateLabel]
preceders = array (minBound, maxBound)
            [ (follows, [ s | s <- labels, canFollow s follows])
            | follows <- labels ]
  where labels = [minBound .. maxBound]

aCostTree :: (MVector NIndex nodes, MVector RIndex residues)
         => BeginNode -> nodes NIndex HMMNode -> residues RIndex AA -> Tree
aCostTree bnode nodes aas = ct Mat (NI $ mvlength nodes) (RI $ mvlength aas)
  where ct stateRight (NI 0) (RI 0) = FromBegin (btransition bnode stateRight)
        ct stateRight (NI 0) ri     = insertAll stateRight ri
        ct stateRight ni (RI 0)     = deleteAll stateRight ni
        ct stateRight ni ri =
          StepFrom [ Scored (state, t) score
                   | state <- preceders ! stateRight -- memoize!
                   , let t = next state ni ri
                   , let score = transition node state stateRight +
                                 emission node state aa
                   ]
          where node = nodes `at` pred ni
                aa = aas `at` pred ri

        next s@Mat ni ri = ct' s (pred ni) (pred ri)
        next s@Del ni ri = ct' s (pred ni) ri
        next s@Ins ni ri = ct' s ni        (pred ri)

        insertAll Del _ = error "this can't happen" -- XXX wrong, needs inf cost
        insertAll stateRight (RI 0) = ct stateRight (NI 0) (RI 0)
        insertAll Mat ri = begin_i_m bnode `addHead` insertAll Ins ri
        insertAll Ins ri =
          StepFrom [Scored (Ins, insertAll Ins (pred ri))
                    (begin_i_i bnode + begin_emissions_i bnode ! aa)]
          where aa = aas `at` pred ri

        deleteAll Ins _ = error "this can't happen" -- XXX wrong, needs inf cost
        deleteAll stateRight (NI 0) = ct stateRight (NI 0) (RI 0)
        deleteAll stateRight ni = 
          StepFrom [Scored (Del, deleteAll Del (pred ni))
                    (transition node Del stateRight)]
            where node = nodes `at` pred ni

        ct' = Memo.memo3 (Memo.arrayRange (minBound, maxBound))
                         (Memo.arrayRange (NI 0, NI $ pred $ mvlength nodes))
                         (Memo.arrayRange (RI 0, RI $ pred $ mvlength aas))
                         ct
          -- N.B. could go with unsafe ranges here
