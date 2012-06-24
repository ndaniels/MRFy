module Perturb
       ( allMovers, decayMovers
       , withStates
       , perturbProps, isPlan7Prop
       )
where
  
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Test.QuickCheck

import HMMProps
import MRFTypes


type State = HMMState

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
-- TESTINB PERTURBATIONS AND RELATED CODE


newtype Plan7 = Plan7 SSeq deriving Show

instance Arbitrary Plan7 where
  shrink (Plan7 states) = map Plan7 $ filter isPlan7 $ shrink states
  arbitrary = fmap Plan7 $ sized $ \n ->
                take <$> choose (0,n) <*> procededBy (const True)
    where procededBy ok = do
            next <- elements $ filter ok [Mat, Ins, Del]
            rest <- procededBy (canFollow next)
            return $ next : rest



rightMoversPermutesProp (Plan7 ss) = all match (rightMoversStates ss)
  where match ss' = sort ss' == sort ss -- big hammer

-- | Predicate @<==>@ judges the equivalence of two Plan7 sequences.
-- N.B. It does not guarantee both lists are Plan7, just that
-- they both agree on whether they are Plan7.

(<==>) :: SSeq -> SSeq -> Bool
ss <==> ss' = agree nodeCount && agree residueCount && agree isPlan7
  where agree f = f ss == f ss'

preservesInvariants f (Plan7 ss) = all (<==> ss) $ take 100000 $ f ss

isPlan7Prop (Plan7 states) = isPlan7 states

perturbProps :: [(String, Property)]
perturbProps = [ ("diagonal", property diagonalProp)
               , ("diagonalCount", property diagonalsCount)
               , ("plan7gen", property isPlan7Prop)
               , ("rightMoversPermutes", property rightMoversPermutesProp)
               , ("allMoversInvariant",
                  property $ preservesInvariants $ withStates allMovers)
               , ("decayMoversInvariant",
                  property $ preservesInvariants $ withStates decayMovers)
               ]
