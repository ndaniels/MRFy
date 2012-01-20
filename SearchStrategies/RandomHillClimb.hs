module SearchStrategies.RandomHillClimb where

import qualified Data.Vector as V
import System.Random (mkStdGen, randomR, StdGen)

import Debug.Trace (trace)

import Beta
import SearchStrategy
import StochasticSearch
import Viterbi

ss :: SearchStrategy
ss = SearchStrategy { accept = accept'
                    , terminate = terminate'
                    , mutate = mutate'
                    , initialize = initialize'
                    }

initialize' :: Seed -> QuerySequence -> [BetaStrand] -> [SearchGuess]
initialize' seed query betas = [initialGuess seed query betas]

accept' :: Seed -> [Score] -> Age -> Bool
accept' _ [] _ = error "go away"
accept' _ [s1] _ = True
accept' _ (s1:s2:scores) _ = s1 < s2

terminate' :: [Score] -> Age -> Bool
terminate' scores age = not $ age < 1000

-- invariant: len [SearchSolution] == 1
mutate' :: Seed -> QuerySequence -> Scorer -> [BetaStrand] -> [SearchSolution] -> [SearchSolution]
mutate' seed query scorer betas solutions = [scorer query betas $ mutate'' guesses 0 $ mkStdGen seed] 
  where guesses = snd $ head solutions

        mutate'' :: SearchGuess -> Int -> StdGen -> SearchGuess
        mutate'' [] _ _ = []
        mutate'' (g:gs) i gen = g' : mutate'' gs (i+1) gen'
          where (g', gen') = randomR (lo, hi) gen
                lo = if i == 0 then
                       1
                     else
                       (len $ (trace "1" $ betas !! (i - 1)))
                       + (trace "2" $ guesses !! (i - 1))
                hi = if i == (length guesses) - 1 then
                       V.length query - (len $ (trace "3" $ betas !! i))
                     else
                       (trace "4" $ guesses !! (i + 1))
                       - (trace "5" $ (len $ betas !! i))

