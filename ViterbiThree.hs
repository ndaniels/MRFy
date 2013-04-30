module ViterbiThree where

import Data.Vector.Unboxed as U hiding (minimum, (++), map)

import qualified Constants as C
import qualified MRFTypes as T
import MRFTypes2
import Score

type StatePath = [ T.StateLabel ]

type QuerySequence = U.Vector C.AA

data Tree = FromBegin Score
          | StepFrom [Scored (T.StateLabel, Tree)]

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

-- Probably all wrong. We're having trouble.
-- The central idea of our new Viterbi with a three-node record type is
-- to traverse the structure of a Model with `mfoldr`, which does case
-- analysis for us on Begin/Middle/End nodes.
--
-- Our *problem* is coming up with a suitable type to accumulate the cost
-- tree. The tree itself is straight forward, but the recursions require
-- additional information for case analysis: the state we're going *TO*
-- and the position of the query sequence.
--
-- *However*, constructing such a value doesn't seem to make sense to us
-- in the base case: the query sequence position and state no longer matter.
-- Therefore it seems sensible to define Iota like so:
--
-- data Iota = Base Tree
--           | Step (Int, StateLabel, Tree -> Tree)
--
-- But then we're right back where we started: case analysis hell.
--
-- Could you shed some light on our predicament?
type Iota = (Int, T.StateLabel, Tree -> Tree)

viterbi :: Model -> QuerySequence -> Tree
viterbi mod qs = mkTree undefined -- wtf
  where (_, _, mkTree) = mfoldr begf medf endf (U.length qs, T.Mat, initTree) mod
        initTree t = StepFrom [ Scored (T.Mat, t) negLogOne -- more wtf
                              , Scored (T.Ins, t) negLogOne
                              , Scored (T.Del, t) negLogOne
                              ]

        begf :: BeginNode -> Iota -> Iota
        begf node (0, stateTo, mkTree) =
          (0, T.Mat, \_ -> mkTree (FromBegin $ beginMatch node stateTo))
        begf node (n, stateTo, mkTree) =
          (0, T.Mat, \_ -> mkTree (insertAll node stateTo n))

        insertAll :: BeginNode -> T.StateLabel -> Int -> Tree
        insertAll bnode stateTo 0 = FromBegin $ beginMatch bnode stateTo
        insertAll bnode T.Del n = error "This can't happen"
        insertAll bnode T.Mat n = logp (b_i_m bnode) `addHead` insertAll bnode T.Ins n
        insertAll bnode T.Ins n =
          StepFrom [Scored (T.Ins, insertAll bnode T.Ins (n - 1))
                           (logp (b_i_i bnode) + binse bnode C./!/ aa)]
          where aa = qs U.! (n - 1)

        beginMatch :: BeginNode -> T.StateLabel -> Score
        beginMatch n T.Mat = logp $ b_m_m n
        beginMatch n T.Ins = logp $ b_m_i n
        beginMatch n T.Del = logp $ b_m_d n

        medf :: MiddleNode -> Iota -> Iota
        medf = undefined

        endf :: EndNode -> Iota -> Iota
        endf = undefined

        logp = T.logProbability


addHead :: Score -> Tree -> Tree
addHead s (FromBegin s') = FromBegin (s + s')
addHead s (StepFrom subtrees) =
  StepFrom [Scored t (s + s') | Scored t s' <- subtrees]




