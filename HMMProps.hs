module HMMProps
where
  
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Debug.Trace (trace)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Random

import CommandArgs (Files(..))
import HMMArby()
import HMMPlus
import MRFTypes
import Score
import Viterbi

hmmProps :: [(String, Property)]
hmmProps = [ ("ubProp", property ubProp)
           , ("buProp", property buProp)
           , ("blockNoMergeProp", property blockNoMergeProp)
           , ("mergeMergeProp", property mergeMergeProp)
           , ("Viterbi admissible", property viterbiAdmissible)
           ]



-- | Predicate tells whether a sequence of states
-- is a legitimate path through a Plan7 Hidden Markov Model
-- (does not include restrictions on @Beg@ and @End@)
isPlan7 :: [StateLabel] -> Bool
isPlan7 = ok
  where ok (Del:Ins:_) = False
        ok (Ins:Del:_) = False
        ok (_:states)  = ok states
        ok []  = True
        
blockIsPlan7 :: [Block StateLabel] -> Bool
blockIsPlan7 = isPlan7 . map state


-- | Counting residues and plan7 nodes
residueCount, nodeCount :: [StateLabel] -> Int
residueCount = sum . map count
  where count Mat = 1
        count Ins = 1
        count _   = 0

-- The final node of the HMM is never considered
-- in the state path, because it's a transition
-- to the non-emitting end state
nodeCount [] = 0
nodeCount xss@(x:xs)
    | x == Ins  = 1 + nodeCount' xss
    | otherwise =     nodeCount' xss
    where nodeCount' = sum . map count
          count Mat  = 1
          count Del  = 1
          count Ins  = 0
          count _    = 0
        
-- | Admissible solution to a problem
-- admissibleSolution :: HMMModel -> QuerySquence -> [StateLabel] -> Bool
admissibleSolution model query states =
  residueCount states == U.length query &&
  nodeCount states == V.length model &&
  isPlan7 states


viterbiAdmissible :: HMM -> QuerySequence -> Bool
viterbiAdmissible model query = admissibleSolution model query soln
  where soln = unScored $ viterbi (:) HasNoEnd query model

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
            , "Score is " ++ show (scoreHMM model query states)
            ]
              where states = unScored $ viterbi (:) HasNoEnd query model

data Block a = Block { state :: a, number :: Int }
  -- ^ invariant, number > 0
  deriving (Eq, Ord, Show)

blockify :: Eq a => [a] -> [Block a]
blockify [] = []
blockify (s:ss) = accum s 1 ss
  where accum cur count [] = [Block cur count]
        accum cur count (s:ss)
          | s == cur  = accum cur (succ count) ss
          | otherwise = Block cur count : accum s 1 ss
                        
unblockify :: [Block a] -> [a]
unblockify = concatMap (\b -> replicate (number b) (state b))

instance (Arbitrary a) => Arbitrary (Block a) where
  arbitrary = do Positive pos <- arbitrary
                 state <- arbitrary
                 return $ Block state (pos `min` 1000)

mergeBlocks :: Eq a => [Block a] -> [Block a]
mergeBlocks (b : bs) | number b == 0 = mergeBlocks bs
mergeBlocks (b1 : b2 : bs)
  | state b1 == state b2 = mergeBlocks (Block (state b1) (number b1 + number b2) : bs)
mergeBlocks (b : bs) = b : mergeBlocks bs
mergeBlocks [] = []

ubProp :: [StateLabel] -> Bool
ubProp ss = (unblockify . blockify) ss == ss
buProp :: [Block StateLabel] -> Bool
buProp bs = (blockify . unblockify) bs == mergeBlocks bs

blockNoMergeProp :: [StateLabel] -> Bool
blockNoMergeProp ss = mergeBlocks blocks == blocks
  where blocks = blockify ss
        
mergeMergeProp :: [Block StateLabel] -> Bool
mergeMergeProp bs = mergeBlocks bs == (mergeBlocks . mergeBlocks) bs




scoreHMM :: HMM -> QuerySequence -> [StateLabel] -> Score
scoreHMM nss qss hss = scoreHMM' (reverse $ V.toList nss) 
                                 (reverse $ U.toList qss)
                                 (reverse hss)
                                 Mat
-- It is now reversed. So we're reading from the end node
-- BUT we have to start with the extra (node n) aScore
-- and we terminate with M, I, or D but must handle node 0 properly
-- we only *see* node zero if its Ins states are present
-- (end) Ins: Ins: Mat: Mat: [](beg)
  where
    scoreHMM' [] [] []         _       = Score 0.0 -- begin
    scoreHMM' (n:[]) [] []     t       = aScore n Mat t
    scoreHMM' (n:[]) (q:[]) (Mat:[]) _ = negLogZero
    scoreHMM' (n:[]) (q:[]) (Del:[]) _ = negLogZero
    scoreHMM' (n:[]) (q:[]) (Ins:[]) _ = eScore n q Ins +
                                         aScore n Mat Ins
    -- -- TODO check all these base cases vs Viterbi
    -- scoreHMM' (n:[]) (q:[]) (Ins:[]) = eScore n q Ins +
    --                                    aScore n (head hs) Ins  +
    --                                    scoreHMM' (n:ns) (q:qs) hs
    scoreHMM' (n:ns) qs (Del:hs) t     = aScore n Del t + 
                                         scoreHMM' ns qs hs Del
    scoreHMM' (n:ns) (q:qs) (Mat:hs) t = eScore n q Mat +
                                         aScore n Mat t +
                                         scoreHMM' ns qs hs Mat
    scoreHMM' (n:ns) (q:qs) (Ins:hs) t = eScore n q Ins +
                                         aScore n Ins t +
                                         scoreHMM' (n:ns) qs hs Ins
    scoreHMM' (n:ns) (q:qs) (_:hs)   t = error "Invalid state"
    scoreHMM' _      _      _        _ = error "WTF"
    -- note: we are assuming we trust the below functions from Viterbi
    eScore = emissionScoreNode
    aScore = transScoreNode

-- next up: perturbing a solution leads to a worse scoring solution
