module HmmProps
where
  
import qualified Data.Vector as V

import HmmPlus
import Viterbi

-- | Predicate tells whether a sequence of states
-- is a legitimate path through a Plan7 Hidden Markov Model
-- (does not include restrictions on @Beg@ and @End@)
isPlan7 :: [HmmState] -> Bool
isPlan7 = ok
  where ok (Del:Ins:_) = False
        ok (Ins:Del:_) = False
        ok (_:states)  = ok states
        ok []  = True
        


-- | Counting residues and plan7 nodes
residueCount, nodeCount :: [HmmState] -> Int
residueCount = sum . map count
  where count Mat = 1
        count Ins = 1
        count _   = 0

nodeCount = sum . map count
  where count Mat = 1
        count Del = 1
        count _   = 0
        
-- | Admissible solution to a problem
-- admissibleSolution :: HMMModel -> QuerySquence -> [HmmState] -> Bool
admissibleSolution model query states =
  residueCount states == V.length query &&
  nodeCount states == V.length model &&
  isPlan7 states


viterbiAdmissible :: HmmModel -> QuerySequence -> Bool
viterbiAdmissible model query = admissibleSolution model query soln
  where soln = unScored $ viterbi (:) (True, True) alpha query model
        alpha = V.empty -- something rotten...

-- next up: perturbing a solution leads to a worse scoring solution
