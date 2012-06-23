module Perturb
       ( perturbProps
       )
where
  
import Data.List
import Data.Maybe
import Test.QuickCheck

import HMMProps
import MRFTypes


type State = HMMState

type SSeq = [State]
type BSeq = [Block State]
type Pred = State -> Bool

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


canFollow Del Ins = False
canFollow Ins Del = False
canFollow _   _   = True

canMix s1 s2 = canFollow s1 s2 && canFollow s2 s1

rightMoversStates, leftMoversStates :: SSeq -> [SSeq]
rightMoversStates = map unblockify . rightMovers (const True) . blockify
leftMoversStates  = map unblockify . leftMovers  (const True) . blockify

rightMovesIntoStates :: State -> SSeq -> [SSeq]
rightMovesIntoStates s = map unblockify . rightMovesInto (const True) s . blockify

allMovers :: BSeq -> [[BSeq]]
allMovers bs = roundRobin $ go [] bs
  where both left' right = map (left' `rejoin`) (rightMovers (okAfter left') right) ++
                           map (`rejoin` right) (rightMovers (okAfter right) left')
        go left' right = both left' right :
                         case right of b:bs -> go (b : left') bs
                                       []   -> []
        okAfter [] = const True
        okAfter (b : _) = canFollow (state b)

rejoin :: [a] -> [a] -> [a]
rejoin xs' ys = foldl (flip (:)) ys xs'
  -- using fold to hope for list fusion

allMoversStates :: SSeq -> [SSeq]
allMoversStates = map unblockify . concat . allMovers . blockify

roundRobin :: [[a]] -> [[a]]
roundRobin [] = []
roundRobin queues = let (heads, tails) = peel queues
                    in  heads : roundRobin tails
  where peel = unzip . catMaybes . map dequeue
        dequeue [] = Nothing
        dequeue (a:as) = Just (a, as)

------------------------------------
-- debugging code

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

decayMovers :: BSeq -> [[BSeq]]
decayMovers bs = map concat $ roundRobin $ go [] bs
  where
    go :: BSeq -> BSeq -> [[[BSeq]]]
    go left' [] = []
    go left' (b : right) = decayB b : go (b : left') right
      where decayB (Block Mat n) = split 0 (n-1) (Ins,Del)
{-
              [split (i-1) (n-i) pair | i <- [1..n], pair <- [(Ins,Del), (Del,Ins)]] ++
-}
            split :: Int -> Int -> (State, State) -> [[BSeq]]
            split k_l k_r (plus, minus) =
              map catMaybes $ diagonals canJoin newLeft' newRight
                where newLeft' = newSide plus k_l left'
                      newRight = newSide minus k_r right
    newSide ion n_matches rest =
      (addBlock ion 1 tail) : rightMovesInto (const True) ion tail
        where tail = addBlock Mat n_matches rest
    merge = map concat . roundRobin

addBlock :: State -> Int -> BSeq -> BSeq
addBlock _ 0 bs = bs
addBlock s n (Block s' n' : bs)
  | s == s' = Block s (n+n') : bs
addBlock s n bs = Block s n : bs

canJoin bs@(Block s _ : _) bs'@(Block s' _ : _)
  | not (canFollow s s') = Nothing
canJoin bs bs' = Just (bs `rejoin` bs')
    
----------------------------------------------------------
--
        
diagonals :: (a -> b -> c) -> [a] -> [b] -> [[c]]
diagonals f (x0:xs) (y0:ys) =
  [f x0 y0] : zipWith (:) (map (f x0) ys) (diagonals f xs (y0:ys))
diagonals _ _ _ = []

diagonalProp (Positive n) = all nematch $ take (n `min` 200) $ diagonals (+) [0..] [0..]
  where nematch (x:xs) = all (==x) xs
        nematch [] = False




------------------------------------
-- properties

newtype Plan7 = Plan7 SSeq

rightMoversPermutesProp (Plan7 ss) = all match (rightMoversStates ss)
  where match ss' = sort ss' == sort ss -- big hammer


perturbProps :: [(String, Property)]
perturbProps = [ ("diagonal", property diagonalProp)
               , ("rightMoversPermutes", property $
                                         (error "waiting on (Arbitrary Plan7)" 
                                         rightMoversPermutesProp :: Gen Bool))
               ]
