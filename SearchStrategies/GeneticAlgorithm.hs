module SearchStrategies.GeneticAlgorithm where

import Data.List
import qualified Data.Vector as V
import System.Random (mkStdGen, randomR, randoms, StdGen)

import Debug.Trace (trace)

import Beta
import Constants
import NonUniform
import SearchStrategy
import Shuffle
import StochasticSearch
import Viterbi

ss :: SearchStrategy
ss = SearchStrategy { accept = accept'
                    , terminate = terminate'
                    , mutate = mutate'
                    , initialize = initialize'
                    }

initialize' :: SearchParameters -> Seed -> QuerySequence -> [BetaStrand] -> [SearchGuess]
initialize' searchP seed query betas = map (\s -> predInitialGuess (getSearchParm searchP secPreds) s query betas) 
                                        $ take (getSearchParm searchP populationSize) rands
  where rands = (randoms (mkStdGen seed)) :: [Int]

accept' :: SearchParameters -> Seed -> [Score] -> Age -> Bool
-- accept' _ _ _ _ = True
accept' _ _ [] _ = error "go away" 
accept' _ _ [s1] _ = True 
accept' _ _ (s1:s2:scores) _ = s1 < s2 

terminate' :: SearchParameters -> [Score] -> Age -> Bool
terminate' searchP scores age = showMe $ not $ age < (generations searchP)
  where showMe = if not $ (10.0 * ((fromIntegral age) / 
                 (fromIntegral (generations searchP)))) `elem` [1.0..10.0] then
                   id
                 else
                   trace ((show age) ++ " generations complete")

-- invariant: len [SearchSolution] == 1
mutate' :: SearchParameters -> Seed -> QuerySequence -> Scorer -> [BetaStrand] -> [SearchSolution] -> [SearchSolution]
mutate' searchP seed query scorer betas solutions = fittest
  where fittest = fst $ shuffle (mkStdGen seed) $ take (getSearchParm searchP populationSize) $ 
            sort $ solutions ++ progeny
        progeny = map (scorer query betas) $
            map (\gs -> mutateChild 0 0 (mkStdGen seed) gs gs) $ getPairings guesses
        guesses = map snd solutions

        mutateChild :: Int -> Int -> StdGen -> SearchGuess -> SearchGuess -> SearchGuess
        mutateChild _ _ _ _ [] = []
        mutateChild i lastGuess gen ogs (g:gs) = g' : mutateChild (i+1) g' gen' ogs gs
          where (g', gen') = randomR (lo, hi) gen
                lo = if i == 0 then
                       0
                     else
                       (len $ (betas !! (i - 1))) + lastGuess
                hi = if i == (length ogs) - 1 then
                       V.length query - (len $ betas !! i)
                     else
                       (ogs !! (i + 1)) - (len $ betas !! i)

getPairings :: [SearchGuess] -> [SearchGuess]
getPairings [] = []
getPairings [p] = [p]
getPairings (p1:p2:ps) = crossover p1 p2 : getPairings ps

-- mutateChild :: StdGen -> SearchGuess -> SearchGuess 
-- mutateChild = mutateChild' 0 0 

crossover :: SearchGuess -> SearchGuess -> SearchGuess
crossover ps qs = sort $ crossover' ps qs

-- invariant: length ps == length qs
crossover' :: SearchGuess -> SearchGuess -> SearchGuess
crossover' [] [] = []
crossover' [p] [q] = if p < q then [p] else [q]
crossover' (p:ps) (q:qs) = leftmost:rightmost:crossover' (init ps) (init qs)
  where leftmost = if p < q then p else q
        rightmost = if lastp > lastq then lastp else lastq
        lastp = last ps
        lastq = last qs

