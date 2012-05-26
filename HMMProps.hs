module HMMProps
where
  
import qualified Data.Vector as V
import Test.QuickCheck
import Test.QuickCheck.Monadic

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

nodeCount = sum . map count
  where count Mat = 1
        count Del = 1
        count _   = 0
        
-- | Admissible solution to a problem
-- admissibleSolution :: HMMModel -> QuerySquence -> [HMMState] -> Bool
admissibleSolution model query states =
  residueCount states == V.length query &&
  nodeCount states == V.length model &&
  isPlan7 states


viterbiAdmissible :: HMM -> QuerySequence -> Bool
viterbiAdmissible model query = admissibleSolution model query soln
  where soln = unScored $ viterbi (:) (True, True) query model

oneTestAdmissible :: (a, HMM, [QuerySequence]) -> Bool
oneTestAdmissible (_, model, queries) = 
   all (viterbiAdmissible model) queries
           
oneTestResults ::  (a, HMM, [QuerySequence]) -> [String]
oneTestResults (_, model, queries) = concatMap (string model) queries
    where string model query =
            [ "Expected " ++ show (V.length query) ++ " residues; got " ++
              show (residueCount states)
            , "Expected " ++ show (V.length model) ++ " nodes; got " ++
              show (nodeCount states)
            , "Solution " ++ (if isPlan7 states then "respects" else "violates") ++
              " Plan7 invariant"]
              where states = unScored $ viterbi (:) (True, True) query model





-- next up: perturbing a solution leads to a worse scoring solution
