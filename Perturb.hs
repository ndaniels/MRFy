module Perturb
       ( allMovers, decayMovers
       , withStates
       , perturbProps, isPlan7Prop
       , oneAllMoversPerturb
       , oneDecayMoversPerturb
       , oneLocalPerturb
       , consistentScoring
       , scoreableMetrics
       , viterbiIsAwesome
       , viterbiFight
       , viterbiFightPath
       , approxEq
       )
where
  
import Control.Applicative
import Control.Monad
import Control.Parallel.Strategies
import Data.Ix
import Data.List
import Data.Maybe
import qualified Data.MemoCombinators as Memo
-- import qualified Data.Set.TernarySet as Set -- cabal troubles
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Debug.Trace (trace)
import Test.QuickCheck
import Test.QuickCheck.Property

import HMMProps
import MRFTypes
import Score
import Viterbi

import Model3 (toHMM, slice, Slice(..), numNodes)
import qualified V4


type State = StateLabel

type SSeq = [State]
type BSeq = [Block State]
type Pred = State -> Bool

------------------------------------------------------------------------------
-- PERTURBATIONS PART I: PURE MOVEMENT


-- | @rightMovers p bs@ returns a list of all the sequences that can be
-- obtained by taking the first element of @bs@ and moving it to the right,
-- subject to the constraint that the first state of the new sequence satisfies 
-- @p@.   No input block may be empty, and no two consecutive blocks may
-- have the same state.  

leftMovers p = map reverse . rightMovers p . reverse

rightMovers, leftMovers :: Pred -> BSeq -> [BSeq]
rightMovers p bs | any ((<=0) . number) bs = error "block size not positive"
rightMovers p []  = []
rightMovers p [b] = []
rightMovers p (Block s n : b2 : bs)
  | not (p s) = error "first state does not satisfy p"
  | n == 1    = rightMovesInto p s (b2:bs)
  | otherwise = map (Block s (pred n) :) $ rightMovesInto (canFollow s) s (b2:bs)
  
rightMovesInto, rightMovesBefore :: Pred -> State -> BSeq -> [BSeq]
  -- if a move can be viewed either as 'into' or 'before',
  -- always treat it as 'before'.  In particular, if the state
  -- is equal to the first state, this is 'before' and not 'into'.
rightMovesInto _ _ [] = []
rightMovesInto ok s (Block s' n : bs)
  | not (ok s') = []
  | otherwise   = if n > 1 && s /= s' && canMix s s' then
                    [Block s' i : Block s 1 : Block s' (n-i) : bs | i <- [1..n-1]]
                    ++ skipThisBlock
                  else
                    skipThisBlock
       where skipThisBlock = map (Block s' n :) (moves (canFollow s') s bs)
             moves = if s == s' then rightMovesInto else rightMovesBefore

rightMovesBefore ok s (Block s' n : bs)
  | ok s && canFollow s s'
           = (mergeBlocks [Block s 1, Block s' n] ++ bs) :
             rightMovesInto ok s (Block s' n : bs)
  | otherwise = rightMovesInto ok s (Block s' n : bs)
rightMovesBefore ok s [] = if ok s then [[Block s 1]] else []


-- | @canFollow s s'@ tells whether one state can follow another 
-- in the Plan7 scheme.  This predicate is symmetric:
-- order doesn't matter.
canFollow Del Ins = False
canFollow Ins Del = False
canFollow _   _   = True

canMix s1 s2 = canFollow s1 s2 && canFollow s2 s1

-- | @allMovers blocks@ returns sequences that can be obtained
-- by allowing any state in any block to move left or right.
-- (Left movement is obtained by right movement of a reversed list.)
-- The sequences are carefully ordered by distance moved.
-- The first element contains all results obtained by a move 
-- of size 1, then the next element contains results of size 2, and so on.
-- N.B. function @withStates allMovers@ will do something similar with
-- a sequence of states.

allMovers :: BSeq -> [[BSeq]]
allMovers bs = roundRobin $ go [] bs
  where both left' right = map (left' `rejoin`) (rightMovers (okAfter left') right) ++
                           map (`rejoin` right) (rightMovers (okAfter right) left')
        go left' right = both left' right :
                         case right of b:bs -> go (b : left') bs
                                       []   -> []
        okAfter [] = const True
        okAfter (b : _) = canFollow (state b)

-- @ @rejoin xs' ys == reverse xs ++ ys@
-- XXX TODO somebody should try both alternative with ghc and see if this 
-- makes a difference
rejoin :: [a] -> [a] -> [a]
rejoin xs' ys = foldl (flip (:)) ys xs'
  -- using fold to hope for list fusion

-- | @roundRobin@ takes a list of queues and returns a list of lists
-- obtained by taking the head of each queue in turn.  We keep taking
-- values until all queues are empty, or if any queue is infinite, forever.
--
-- RoundRobin might be quite similar to List.transpose, but we don't
-- know what guarantees @transpose@ provides for irregular lists.

roundRobin :: [[a]] -> [[a]]
roundRobin [] = []
roundRobin queues = let (heads, tails) = peel queues
                    in  heads : roundRobin tails
  where peel = unzip . catMaybes . map dequeue
        dequeue [] = Nothing
        dequeue (a:as) = Just (a, as)

-----------------------------------
-- Lifting block computations to state computations

-- | Combinators for transforming block operations to sequence operations.
-- The tags help ensure we got the distance stuff approximately right.
withStates       :: (BSeq -> [[BSeq]]) -> (SSeq -> [SSeq])
withTaggedStates :: (BSeq -> [[BSeq]]) -> (SSeq -> [(SSeq, Int)])

withStates movers = map unblockify . concat . movers . blockify
withTaggedStates movers =
  map (leftMap unblockify) . concat . zipWith tag [1..] . movers . blockify 
  where tag n bs = map (\b -> (b,n)) bs
        leftMap f (x, y) = (f x, y)
        


------------------------------------
-- debugging code


rightMoversStates, leftMoversStates :: SSeq -> [SSeq]
rightMoversStates = map unblockify . rightMovers (const True) . blockify
leftMoversStates  = map unblockify . leftMovers  (const True) . blockify

rightMovesIntoStates :: State -> SSeq -> [SSeq]
rightMovesIntoStates s = map unblockify . rightMovesInto (const True) s . blockify

allRightMoversStates :: SSeq -> [(SSeq, Int)]
allRightMoversStates = map (mapLeft unblockify) . concat . allRightMovers . blockify
  where mapLeft f (a, b) = (f a, b)

allRightMovers :: BSeq -> [[(BSeq, Int)]]
allRightMovers bs = roundRobin $ go [] bs
  where movers left' right = map (rejoin left') (rightMovers (okAfter left') right)
        go left' right = zip (movers left' right) [1..] :
                         case right of b:bs -> go (b : left') bs
                                       []   -> []
        okAfter [] = const True
        okAfter (b : _) = canFollow (state b)

------------------------------------------------------------------------------
-- PERTURBATIONS PART II: PARTICLE DECAY FOLLOWED BY MOVEMENT

-- | @decyMovers blocks@ returns sequences that can be obtained
-- by allowing any Match state in any block to decay into an I/D or D/I pair
-- of 'ions'.  The ions then move left or right as permitted by the
-- plan7 invariants.  (At least one ion must move, otherwise we have
-- adjacent ions.)

decayMovers :: BSeq -> [[BSeq]]
decayMovers bs = map concat $ roundRobin $ go [] bs
  where
    go :: BSeq -> BSeq -> [[[BSeq]]]
    go left' [] = []
    go left' (b : right) = decayB b $ go (b : left') right
      where decayB (Block Mat n) tail =
              [ block | i <- [1..n], ions <- [(Ins,Del), (Del,Ins)]
                      , block <- split (i-1) (n-i) ions ] : tail
            decayB _ tail = tail

            split :: Int -> Int -> (State, State) -> [[BSeq]]
            split k_l k_r (plus, minus) =
              map catMaybes $ diagonals canJoin newLeft' newRight
                where newLeft' = newSide plus k_l left'
                      newRight = newSide minus k_r right


    -- the functions below are all definable at top level,
    -- but they are auxiliary functions for decayMovers

    newSide ion n_matches rest =
      addIon ion tail `consMaybe` rightMovesInto (const True) ion tail
        where tail = addBlock Mat n_matches rest

    consMaybe Nothing xs = xs
    consMaybe (Just x) xs = x : xs

    addIon :: State -> BSeq -> Maybe BSeq
    addIon s [] = Just [Block s 1]
    addIon s (Block s' n : bs)
      | s == s' = Just (Block s (succ n) : bs)
      | canFollow s s' = Just (Block s 1 : Block s' n : bs)
      | otherwise = Nothing

    addBlock :: State -> Int -> BSeq -> BSeq
    addBlock _ 0 bs = bs
    addBlock s n (Block s' n' : bs)
      | s == s' = Block s (n+n') : bs
    addBlock s n bs = Block s n : bs

    canJoin (Block s _ : _) (Block s' _ : _)
      | not (canFollow s s') = Nothing
    canJoin bs bs' = Just (bs `rejoin` bs')
    
----------------------------------------------------------
--
-- code to get distances right when summing left and right moves
        
diagonals :: (a -> b -> c) -> [a] -> [b] -> [[c]]
diagonals f (x0:xs) (y0:ys) =
  [f x0 y0] : zipCons (map (f x0) ys) (diagonals f xs (y0:ys))
diagonals _ _ _ = []

zipCons (x:xs) (ys:yss) = (x:ys) : zipCons xs yss
zipCons []     yss      = yss
zipCons xs     []       = map (\xs -> [xs]) xs


diagonalProp (Positive n) = all nematch $ take (n `min` 200) $ diagonals (+) [0..] [0..]
  where nematch (x:xs) = all (==x) xs
        nematch [] = False


diagonalsCount :: Positive Int -> Positive Int -> Bool
diagonalsCount (Positive n') (Positive m') =
  length (diagonals (\_ _ -> ()) [1..n] [1..m]) == m + n - 1
    where m = min m' 300
          n = min m' 300
 
-----------------------------------------------------------------------------------
-- GENERATING ALL PLAN7 SEQUENCES FOR A GIVEN METRIC

data Metrics = M { nNodes :: Int, nResidues :: Int }
  deriving (Eq, Ord, Ix, Show)

minus :: Metrics -> State -> Metrics
minus (M nCnt rCnt) Mat = M (pred nCnt) (pred rCnt)
minus (M nCnt rCnt) Ins = M nCnt (pred rCnt)
minus (M nCnt rCnt) Del = M (pred nCnt) rCnt

minusBeginningIns :: Metrics -> State -> Metrics
minusBeginningIns (M nCnt rCnt) Ins = M (pred nCnt) (pred rCnt)
minusBeginningIns metrics state = metrics `minus` state

nonnegative :: Metrics -> Bool
nonnegative (M nCnt rCnt) = nCnt >= 0 && rCnt >= 0

instance Arbitrary Metrics where
  arbitrary = M <$> choose (0, 8) <*> choose (0, 8)
  shrink (M n r) = map (uncurry M) $ shrink (n, r)


allp7 :: Metrics -> [SSeq]
allp7 metrics = allp7' metrics Mat minusBeginningIns -- any state can follow Mat
  where
    -- @allp7' metrics prev@ generates all sequences of states with the
    -- given metrics, provided that if the sequence is not empty, it
    -- begins with a state that can follow @prev@.  For the first state
    -- @prev@ is @Mat@, which is blatantly horrible, but it works because
    -- any state can follow @Mat@.  We'd prefer to use a predicate, but
    -- that makes memoziation ugly.  
    allp7' :: Metrics -> State -> (Metrics -> State -> Metrics) -> [SSeq]
    allp7' (M 0 0) _ _ = return []
    allp7' metrics prev nextMinus =
        [ s : ss | s <- filter (canFollow prev) [Mat, Ins, Del]
                 , nonnegative (metrics `nextMinus` s)
                 , ss <- allp7memo (metrics `nextMinus` s) s minus ]

    allp7memo :: Metrics -> State -> (Metrics -> State -> Metrics) -> [SSeq]
    allp7memo =
      Memo.memo2 (Memo.arrayRange (M 0 0, metrics))
                 (Memo.arrayRange (Mat, Del))
                 allp7'

noP7Dups :: Metrics -> Bool
noP7Dups ms = length (nub ss) == length ss
  where ss = allp7 ms

approxEq :: Score -> Score -> Bool
approxEq (Score x) (Score x') = abs (x - x') < epsilon
   where epsilon = 0.0000001

consistentScoring :: HMM -> QuerySequence -> Property
consistentScoring model query = printTestCase msg $
  scoreOf vscored `approxEq` hmmScore
  where vscored = viterbi (:) HasNoEnd query model
        hmmScore = scoreHMM model query (unScored vscored)
        msg = unlines [ "Viterbi score " ++ show (scoreOf vscored)
                      , "Viterbi solution " ++ showSS (unScored vscored)
                      , "HMM score " ++ show hmmScore
                      ]

showSS :: SSeq -> String
showSS = map (head . show)

goodMetrics :: Metrics -> Bool
goodMetrics m@(M nCnt rCnt) = all ok $ allp7 m
  where ok ss = nodeCount ss == nCnt && residueCount ss == rCnt

scoreableMetrics :: HMM -> QuerySequence -> Bool
scoreableMetrics model query = all id $ (parMap rseq) goodScore $ allp7 metrics
  where goodScore ss =
          let score = (scoreHMM model query ss)
           in score >= 0 || score <= 0 -- it's good to be bad
        metrics = M (V.length model) (U.length query)

viterbiIsAwesome :: HMM -> QuerySequence -> Bool
viterbiIsAwesome model query = 
    all ((scoreOf vscored) <=) possibleScores
  where vscored = viterbi (:) HasNoEnd query model
        possibleScores = (parMap rseq) (scoreHMM model query) allStates
        allStates = allp7 $ M n r
        _message =  ("\n\nViterbi: " ++ (show $ scoreOf vscored) ++ 
         "\nViterbi SSeq: " ++ (show $ unScored vscored) ++
         "\n\nPlan7 Gen scores: " ++ (show possibleScores) ++
         "\nPlan7 Gen SSeqs: " ++ (show allStates))


        -- If the state sequence starts with an insertion, that insert
        -- comes from node 0. *Otherwise*, node 0 is not represented in
        -- the state sequence. This is difficult to represent here; perhaps
        -- it should go in the generator? How?
        (n, r) = (V.length model, U.length query)

viterbiFight :: HMM -> QuerySequence -> Bool
viterbiFight ohmm query = abs (oscore - nscore) < 0.00001
  where oscore = unScore $ scoreOf $ viterbi consNoPath HasNoEnd query ohmm
        nscore = unScore $ V4.scoreOnly model query
        model = slice hmm (Slice { nodes_skipped = 0, width = numNodes hmm })
        hmm = toHMM ohmm

viterbiFightPath :: HMM -> QuerySequence -> Bool
viterbiFightPath ohmm query = all stateEq $ zip opath npath
  where opath = unScored $ viterbi consPath HasNoEnd query ohmm
        npath = unScored $ V4.statePath $ V4.inlinedTree model query
        model = slice hmm (Slice { nodes_skipped = 0, width = numNodes hmm })
        hmm = toHMM ohmm

        stateEq :: (StateLabel, V4.StateLabel) -> Bool
        stateEq (Mat, V4.Mat) = True
        stateEq (Ins, V4.Ins) = True
        stateEq (Del, V4.Del) = True
        stateEq (_  , _     ) = False

traceid :: Show a => String -> a -> a
traceid prefix a = trace (prefix ++ ": " ++ (show a)) a

-----------------------------------------------------------------------------------
-- TESTS ON REAL DATA

-- I'm not sure how to grab a witness. :-/
oneAllMoversPerturb :: (a, HMM, [QuerySequence]) -> String
oneAllMoversPerturb (_, model, queries) = 
  if all pass queries then
    "all-perturb PASSED"
  else
    "all-perturb FAILED"
  where pass query = optimal (withStates allMovers) model query

oneDecayMoversPerturb :: (a, HMM, [QuerySequence]) -> String
oneDecayMoversPerturb (_, model, queries) = 
  if all pass queries then
    "decay-perturb PASSED"
  else
    "decay-perturb FAILED"
  where pass query = optimal (withStates decayMovers) model query

-- This is broken. Bandwidth allotment exceeded.
-- I'm finding it difficult to debug (I needed witnesses).
oneLocalPerturb :: (a, HMM, [QuerySequence]) -> String
oneLocalPerturb (_, model, queries) = 
  if all pass queries then
    "local-perturb PASSED"
  else
    "local-perturb FAILED"
  where pass query = all (states <==>) $ viterbiLocalPerturb states
          where states = unScored $ viterbi (:) HasNoEnd query model

        viterbiLocalPerturb :: [StateLabel] -> [[StateLabel]]
        viterbiLocalPerturb states = trace (show $ head perturbs) perturbs
          where perturbs = nTimes tx 10 states

        nTimes :: ([a] -> [a]) -> Int -> [a] -> [[a]]
        nTimes tx 0 as = [as]
        nTimes _ _ [] = []
        nTimes tx n (a1:a2:a3:a4:as) =
          [a1:a2:a3:a4:bs | bs <- nTimes tx n as] ++
          [(tx (a1:a2:a3:a4:[])) ++ bs | bs <- nTimes tx (pred n) as]
        nTimes tx n (_:as) = [as]

        tx :: [StateLabel] -> [StateLabel]
        tx (Mat:Mat:Mat:Mat:[]) = [Mat, Ins, Mat, Del, Mat]
        tx (Ins:Mat:Mat:Del:[]) = [Del, Del, Mat, Ins, Ins]
        tx (Mat:Mat:Mat:Del:[]) = [Mat, Ins, Mat, Del, Del]
        tx (Del:Mat:Mat:Mat:[]) = [Del, Del, Mat, Ins, Mat]
        tx (Mat:Mat:Mat:Ins:[]) = [Mat, Del, Mat, Ins, Ins]
        tx (Ins:Mat:Mat:Mat:[]) = [Ins, Ins, Mat, Del, Mat]
        

-----------------------------------------------------------------------------------
-- TESTINB PERTURBATIONS AND RELATED CODE


newtype Plan7 = Plan7 SSeq deriving Show

instance Arbitrary Plan7 where
  shrink (Plan7 states) = map Plan7 $ filter isPlan7 $ shrink states
  arbitrary = fmap Plan7 $ sized $ \n ->
                take <$> choose (0,n) <*> states (const True)
    where -- | Calling @states p@ generates an infinite Plan7 sequence of
          -- states where the first state satisfies @p@.
          states :: (State -> Bool) -> Gen [State]
          states ok = do
            next <- elements $ filter ok [Mat, Ins, Del]
            rest <- states (canFollow next)
            return $ next : rest



rightMoversPermutesProp (Plan7 ss) = all match (rightMoversStates ss)
  where match ss' = sort ss' == sort ss -- big hammer

optimal :: (SSeq -> [SSeq]) -> HMM -> QuerySequence -> Bool
optimal f model query = all (score <) $ map (scoreHMM model query) $ f states
  where (states, score) = (unScored v, scoreOf v)
        v = viterbi (:) HasNoEnd query model

-- | Predicate @<==>@ judges the equivalence of two Plan7 sequences.
-- N.B. It does not guarantee both lists are Plan7, just that
-- they both agree on whether they are Plan7.

(<==>) :: SSeq -> SSeq -> Bool
ss <==> ss' = agree nodeCount && agree residueCount && agree isPlan7
  where agree f = f ss == f ss'

preservesInvariants f (Plan7 ss) = all (<==> ss) $ take 100000 $ f ss

isPlan7Prop (Plan7 states) = isPlan7 states

distinctPerturbations p1 p2 (Plan7 ss) =
  disjoint (Set.fromList $ concat $ p1 bs) (Set.fromList $ concat $ p2 bs)
  where bs = blockify ss
        disjoint s s' = Set.null $ s `Set.intersection` s'

shrinkPred p = fmap (all p . shrink) arbitrary

perturbProps :: [(String, Property)]
perturbProps = [ ("diagonal", property diagonalProp)
               , ("diagonalCount", property diagonalsCount)
               , ("good-metrics", property goodMetrics)
               , ("nop7dups", property noP7Dups)
               , ("plan7gen", property isPlan7Prop)
               , ("plan7gen-shrink", property $ shrinkPred $ isPlan7Prop)
               , ("rightMoversPermutes", property rightMoversPermutesProp)
               , ("allMoversInvariant",
                  property $ preservesInvariants $ withStates allMovers)
               , ("decayMoversInvariant",
                  property $ preservesInvariants $ withStates decayMovers)
               , ("distinct-movers-decay",
                  property $ distinctPerturbations allMovers decayMovers)
               , ("viterbi-awesome", property viterbiIsAwesome)
               ]

