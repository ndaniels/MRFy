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
rightMovesInto _ _ [] = []
rightMovesInto ok s (Block s' n : bs)
  | not (ok s') = []
  | s == s'     = (Block s' (succ n) : bs) :
                  map (Block s' n :) (rightMovesBefore (canFollow s') s bs)
  | otherwise   = if n > 1 && canMix s s' then
                    [Block s' i : Block s 1 : Block s' (n-1) : bs | i <- [1..n-1]]
                    ++ skipThisBlock
                  else
                    skipThisBlock
       where skipThisBlock = map (Block s' n :) (rightMovesBefore (canFollow s') s bs)

rightMovesBefore ok s (Block s' n : bs)
  | ok s   = (mergeBlocks [Block s 1, Block s' n] ++ bs) :
             rightMovesInto ok s (Block s' n : bs)
  | ok s'  = rightMovesInto ok s (Block s' n : bs)
  | otherwise = []
rightMovesBefore ok s [] = if ok s then [[Block s 1]] else []


canFollow Del Ins = False
canFollow Ins Del = False
canFollow _   _   = True

canMix s1 s2 = canFollow s1 s2 && canFollow s2 s1

rightMoversStates :: SSeq -> [SSeq]
rightMoversStates = map unblockify . rightMovers (const True) . blockify

rightMovesIntoStates :: State -> SSeq -> [SSeq]
rightMovesIntoStates s = map unblockify . rightMovesInto (const True) s . blockify

allMovers' :: BSeq -> [[BSeq]]
allMovers' bs = roundRobin $ go [] bs
  where movers left' right = rightMovers (okAfter left')  right ++
                             leftMovers  (okBefore right) left'
        go left' right = movers left' right :
                         case right of b:bs -> go (b : left') bs
                                       []   -> []
        okAfter [] = const True
        okAfter (b : _) = canFollow (state b)
        okBefore = okAfter -- everything is reversible

allMoversStates :: SSeq -> [SSeq]
allMoversStates = map unblockify . concat . allMovers' . blockify
  where mapLeft f (a, b) = (f a, b)

roundRobin :: [[a]] -> [[a]]
roundRobin [] = []
roundRobin queues = let (heads, tails) = peel queues
                    in  heads : roundRobin tails
  where peel = unzip . catMaybes . map dequeue
        dequeue [] = Nothing
        dequeue (a:as) = Just (a, as)

------------------------------------



newtype Plan7 = Plan7 SSeq

rightMoversPermutesProp (Plan7 ss) = all match (rightMoversStates ss)
  where match ss' = sort ss' == sort ss -- big hammer


{- needs Arbitrary Plan7
perturbProps :: [(String, Property)]
perturbProps = [ ("rightMoversPermutes", property rightMoversPermutesProp)
               ]
-}
perturbProps = error "waiting on (Arbitrary Plan7)"
