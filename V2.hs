{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module V2
       ( Tree(..), AA(..), StateLabel(..), FullStateLabel(..)
       , HMMNode(..), BeginNode(..)
       , costTree, canFollow
       )
where

--import qualified Data.MemoCombinators as Memo

import Data.Array
import Score


newtype AA = AA Int
             deriving (Eq, Ord, Ix)

data StateLabel = Mat | Ins | Del
                deriving (Eq, Ord, Enum, Show, Ix)
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
                   | state <- [Mat .. Del], canFollow state stateRight
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

        ct' = ct -- memoize here (will require length parms)

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
