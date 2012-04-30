module HmmProps
 ()
where
  
import HmmPlus

-- | Predicate tells whether a sequence of states
-- is a legitimate path through a Plan7 Hidden Markov Model
-- (does not include restrictions on @Beg@ and @End@)
isPlan7 :: [HmmState] -> Bool
isPlan7 = ok
  where ok (Del:Ins:_) = False
        ok (Ins:Del:_) = False
        ok (_:states)  = ok states
        ok []  = True
        


-- | Counting residues and states
residueCount, stateCount :: [HmmState] -> Int
residueCount = sum . map count
  where count Mat = 1
        count Ins = 1
        count _   = 0

stateCount = sum . map count
  where count Mat = 1
        count Del = 1
        count _   = 0
        
-- | Admissible solution to a problem
-- admissibleSolution :: HMMModel -> QuerySquenc