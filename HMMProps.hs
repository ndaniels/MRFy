module HMMProps
where
  
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Debug.Trace (trace)
import CommandArgs (Files(..))
import HMMPlus
import MRFTypes
import Score
import Viterbi

-- | Predicate tells whether a sequence of states
-- is a legitimate path through a Plan7 Hidden Markov Model
-- (does not include restrictions on @Beg@ and @End@)
isPlan7 :: [HMMState] -> Bool
isPlan7 = ok
  where ok (Del:Ins:_) = False
        ok (Ins:Del:_) = False
        ok (_:states)  = ok states
        ok []  = True
        


-- | Counting residues and plan7 nodes
residueCount, nodeCount :: [HMMState] -> Int
residueCount = sum . map count
  where count Mat = 1
        count Ins = 1
        count _   = 0

-- The final node of the HMM is never considered
-- in the state path, because it's a transition
-- to the non-emitting end state
nodeCount = ((+) 1) . sum . map count
  where count Mat = 1
        count Del = 1
        count Ins = 0
        count _   = 0
        
-- | Admissible solution to a problem
-- admissibleSolution :: HMMModel -> QuerySquence -> [HMMState] -> Bool
admissibleSolution model query states =
  residueCount states == U.length query &&
  nodeCount states == V.length model &&
  isPlan7 states


viterbiAdmissible :: HMM -> QuerySequence -> Bool
viterbiAdmissible model query = admissibleSolution model query soln
  where soln = unScored $ viterbi (:) (False, False) query model

oneTestAdmissible :: (a, HMM, [QuerySequence]) -> Bool
oneTestAdmissible (_, model, queries) = 
   all (viterbiAdmissible model) queries
           
oneTestResults ::  (a, HMM, [QuerySequence]) -> [String]
oneTestResults (_, model, queries) = concatMap (string model) queries
    where string model query =
            [ "Expected " ++ show (U.length query) ++ " residues; got " ++
              show (residueCount states)
            , "Expected " ++ show (V.length model) ++ " nodes; got " ++
              show (nodeCount states)
            , "Solution " ++ (if isPlan7 states then "respects" else "violates") ++
              " Plan7 invariant"
           -- , "Score is " ++ show (scoreHMM model query states)
            ]
              where states = unScored $ viterbi (:) (False, False) query model



-- scoreHMM :: HMM -> QuerySequence -> [HMMState] -> Score
-- scoreHMM nss qss hss = scoreHMM' (reverse $ V.toList nss) 
--                                  (reverse $ V.toList qss)
--                                  (reverse hss)
-- -- ok, so reverse it
-- -- then we have to start with the extra (node n) aScore
-- -- and we terminate with M, I, or D but handle node 0 properly
--   where
--     scoreHMM' [] [] []               = Score 0.0
--     scoreHMM' (n:[]) (q:[]) (Mat:[]) = eScore n q Mat +
--                                        aScore n 
--     scoreHMM' (n:[]) (q:[]) (Del:[]) = aScore Mat Del n
--     scoreHMM' (n:[]) (q:[]) (Ins:[]) = eScore n q Ins +
--                                        aScore Mat Ins n
--     -- -- TODO check all these base cases vs Viterbi
--     -- scoreHMM' (n:[]) (q:[]) (Ins:[]) = eScore n q Ins +
--     --                                    aScore n (head hs) Ins  +
--     --                                    scoreHMM' (n:ns) (q:qs) hs
--     scoreHMM' (n:ns) (q:qs) (Del:hs) = aScore (head ns) (head hs) Del + 
--                                        scoreHMM' (n:ns) qs  hs
--     scoreHMM' (n:ns) (q:qs) (Mat:hs) = eScore n q Mat +
--                                        aScore (head ns) (head hs) Mat +
--                                        scoreHMM'  ns    (q:qs) hs
--     scoreHMM' (n:ns) (q:qs) (Ins:hs) = eScore n q Ins +
--                                        aScore n (head hs) Ins +
--                                        scoreHMM' (n:ns) (q:qs) hs
--     scoreHMM' (n:ns) (q:qs) (Del:hs) = aScore (head ns) (head hs) Del + 
--                                        scoreHMM' (n:ns) qs  hs
--     scoreHMM' (n:ns) (q:qs) (_  :hs) = error "Invalid state"
--     scoreHMM' _      _      _        = error "WTF"
--     -- note: we are assuming we trust the below functions from Viterbi
--     eScore = emissionScoreNode
--     aScore = transScoreNode

-- next up: perturbing a solution leads to a worse scoring solution
