module SearchStrategy where

import qualified Data.List as DL
import qualified Data.Vector as V
import System.Random (mkStdGen, Random, random, randomR, randoms, StdGen)

import Beta
import NonUniform
import PsiPred
import StochasticSearch
import Viterbi

import Debug.Trace (trace)

initialGuess :: Seed -> QuerySequence -> [BetaStrand] -> SearchGuess
initialGuess seed qs betas = initialGuess' betas 0 $ mkStdGen seed
  where initialGuess' :: [BetaStrand] -> Int -> StdGen -> SearchGuess
        initialGuess' [] _ _ = []
        initialGuess' (b:bs) lastGuess gen = pos : initialGuess' bs (pos + len b) gen'
          where (pos, gen') = randomR (lastGuess, betaSum) gen
                betaSum = V.length qs - (foldr (+) 0 $ map len (b:bs))

geoInitialGuess :: Seed -> QuerySequence -> [BetaStrand] -> SearchGuess
geoInitialGuess seed qs betas = initialGuess' betas 0 $ mkStdGen seed
  where initialGuess' :: [BetaStrand] -> Int -> StdGen -> SearchGuess
        initialGuess' [] _ _ = []
        initialGuess' (b:bs) lastGuess gen = pos : initialGuess' bs (pos + len b) gen'
          where pos = head rand
                betaSum = V.length qs - (foldr (+) 0 $ map len (b:bs))
                (seed, gen') = random gen

                rand = (randomsDist (mkStdGen seed) 
                        $ zip [lastGuess..] 
                        $ map ((/) 1.0) 
                        $ take (betaSum - lastGuess)
                        $ map (** 1) ([1.0, 2.0..] :: [Double])) :: [Int]

-- predInitialGuess :: [SSPrediction] -> Seed -> QuerySequence -> [BetaStrand] -> SearchGuess 
-- predInitialGuess preds seed qs betas = initialGuess' betas 0 $ mkStdGen seed 
  -- where initialGuess' :: [BetaStrand] -> Int -> StdGen -> SearchGuess 
        -- initialGuess' [] _ _ = [] 
        -- initialGuess' (b:bs) lastGuess gen = pos : initialGuess' bs (pos + len b) gen' 
          -- where pos = head rand 
                -- betaSum = V.length qs - (foldr (+) 0 $ map len (b:bs)) 
                -- (seed, gen') = random gen 
--  
                -- rand = randomsDist (mkStdGen seed) 
                        -- $ take (betaSum - lastGuess) 
                        -- $ drop (lastGuess - 1) 
                        -- $ map (\p -> (residueNum p, beta_score p)) preds 

predInitialGuess :: [SSPrediction] -> Seed -> QuerySequence -> [BetaStrand] -> SearchGuess
predInitialGuess preds seed qs betas = initialGuess' $ randoms $ mkStdGen seed
  where initialGuess' :: [Int] -> SearchGuess
        initialGuess' [] = error "IMPOSSIBLE!"
        initialGuess' (r:rs) = trace ((show guess) ++ " --- " ++ (show $ checkGuess betas guess)) $ if checkGuess betas guess then guess else initialGuess' rs
          where guess = genGuess r (map (\p -> (residueNum p, beta_score p)) preds) $ length betas

genGuess :: (Random r, Fractional r, Ord r) => Seed -> [(Int, r)] -> Int -> SearchGuess
genGuess seed dist n = DL.sort $ take n $ randomsDist (mkStdGen seed) dist

checkGuess :: [BetaStrand] -> SearchGuess -> Bool
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