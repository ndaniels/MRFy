{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module SearchStrategy where

import Control.Monad.LazyRandom
import qualified Data.List as DL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random (mkStdGen, Random, random, randomR, randoms, StdGen)
import Test.QuickCheck

import Beta
import HMMPlus
import qualified HyperTriangles as HT
import LazySearchModel
import MRFTypes
import NonUniform
import PsiPred
import Score
import StochasticSearch
import Viterbi

import Debug.Trace (trace)


-- | @isTick N t k@ is a clock that ticks @t@ times as 
-- @k@ ranges over the interval [1..N].  It's used for
-- emitting diagnostic output at regular intervals of a search
isTick denom numIntervals num =
  num * numIntervals `div` denom > (num - 1) * numIntervals `div` denom

tickProp (Positive n') (Positive t) =
  (length $ filter id $ map (isTick n t) [1..n]) == t
  where n = n' + t

-- | @takeNGenerations n generations@ returns the history of the
-- useful states found in @take n generations@.  It would be nice
-- to use @take@ and @catMaybes@ here, but the function has to
-- make noise, too.
takeNGenerations :: Int -> [AUS a] -> History a
takeNGenerations n = take emptyHistory
  where take (!older) (gen : younger) =
          if showMe (ccost < n) then
            take (gen `extendUsefulHistory` older) younger
          else
            older
          where ccost = ccostOf gen
                showMe = if isTick ccost 10 n then
                           trace (show ccost ++ " generations complete")
                         else
                           id

-- | @takeByCCostGap p pops@ accumulates from @pops@
-- as long as @p older younger@ is @True@, where
-- @older@ is the age of most recent *accepted* population
-- and @younger@ is the age of the *current* population
takeByCCostGap :: (CCost -> CCost -> Bool) -> [AUS a] -> History a
takeByCCostGap continue = take emptyHistory
  where take (older @ (History (pop : _))) (gen : younger) 
          | not (continue (ccostOf pop) (ccostOf gen)) = older
        take (!older) (gen : younger) = take (gen `extendUsefulHistory` older) younger




-- | @acceptableCCostGap searchP@ returns a function that tells if
-- an age gap is acceptable according to the search parameters.
-- If no maximum gap is specified, then *every* gap is acceptable.
acceptableCCostGap :: SearchParameters -> CCost -> CCost -> Bool
acceptableCCostGap searchP a age = maybe True (age - a <=) (convergenceAge searchP)
                                 && age < generations searchP




type InitialGuesser r
 = HMM -> [SSPrediction] -> QuerySequence -> [BetaStrand] -> Rand r Placement

initialGuess :: forall r . RandomGen r => InitialGuesser r
initialGuess _ _ qs betas = initialGuess' betas 0
  where initialGuess' :: [BetaStrand] -> Int -> Rand r Placement
        initialGuess' [] _ = return []
        initialGuess' (b:bs) lastGuess = do
          pos <- getRandomR (lastGuess, betaSum) 
          initialGuess' bs (pos + len b) >>= return . (pos :)
          where betaSum = U.length qs - sum (map len (b:bs))

{-
geoInitialGuess :: InitialGuesser r
geoInitialGuess _ _ seed qs betas = initialGuess' betas 0
  where initialGuess' :: [BetaStrand] -> Int -> StdGen -> Placement
        initialGuess' [] _ _ = []
        initialGuess' (b:bs) lastGuess gen = pos : initialGuess' bs (pos + len b) gen'
          where pos = head rand
                betaSum = U.length qs - (foldr (+) 0 $ map len (b:bs))
                (seed, gen') = random gen

                rand = (randomsDist (mkStdGen seed) 
                        $ zip [lastGuess..] 
                        $ map ((/) 1.0) 
                        $ take (betaSum - lastGuess)
                        $ map (** 1) ([1.0, 2.0..] :: [Double])) :: [Int]
-}

{-
predInitialGuess :: InitialGuesser
predInitialGuess _ preds seed qs betas = initialGuess' $ randoms $ mkStdGen seed
  where initialGuess' :: [Int] -> Placement
        initialGuess' [] = error "IMPOSSIBLE!"
        initialGuess' (r:rs) = trace ((show guess) ++ " --- " ++ (show $ checkGuess betas guess)) $ if checkGuess betas guess then guess else initialGuess' rs
          where guess = genGuess r (map (\p -> (residueNum p, beta_score p)) preds) $ le
ngth betas


genGuess :: (Random r, Fractional r, Ord r) => Seed -> [(Int, r)] -> Int -> Placement
genGuess seed dist n = DL.sort $ take n $ randomsDist (mkStdGen seed) dist
-}

projInitialGuess :: RandomGen r => InitialGuesser r
projInitialGuess hmm _ qs betas = return $ initialGuess' betas 0 0
  where initialGuess' [] _ _ = []
  -- trace ("g: " ++ show g ++ " f: " ++ show f ++ " pos: " ++ show pos ++ " lastB: " ++ show lastB ++ " lastP: " ++ show lastP) $
        initialGuess' (b:bs) lastP lastB = g : initialGuess' bs g pos
          where g = lastP + (floor $ (fromIntegral $ pos - lastB) * f)
                f = (fromIntegral $ U.length qs) / (fromIntegral $ V.length hmm)
                pos = resPosition $ head $ residues b

-- XXX name? contract?
checkGuess :: [BetaStrand] -> Placement -> Bool
checkGuess [] [] = True
checkGuess [] [g] = False
checkGuess [b] [] = False
checkGuess (b:bs) (g:gs) = noClash && next
  where next = if noClash then checkGuess bs gs else False
        noClash = case gs of
                    [] -> True
                    (g':gs') -> g' > g + len b

-- Notes for new initial guess approach:
-- Greedily consume area under curve proportional to beta strand fraction of total beta length
-- If the number of residues inadequate, expand area until we have enough residues
-- generate placement of beta strand start in range from beginning of partition to (end - len beta)
-- Move start of next position left to end of this beta
-- This is generative because we can violate constraints.

-- other approaches: 
-- projection
-- projection with mutation
-- distribution of gaps



--------------------------
-- | Chooses every possible placement with equal probability
equalPlacement :: RandomGen r => QuerySequence -> [BetaStrand] -> Rand r Placement
equalPlacement query betas = do
  gap : gaps <- HT.pointInTri (HT.D nGaps) (HT.L width)
  return $ place gap betas gaps
  where nGaps = length betas + 1
        width = U.length query - sum (map len betas)
        place i (beta:betas) (gapAfter:gaps) =
          i : place (i+len beta+gapAfter) betas gaps
        place i [] [] = if i == U.length query then []
                        else error "gaps in equalPlacement don't add up"
