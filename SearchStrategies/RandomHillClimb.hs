module SearchStrategies.RandomHillClimb where

import qualified Data.Vector as V
import System.Random (mkStdGen, randomR, StdGen)

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
accept' _ (s1:s2:scores) _ = s1 < s2

terminate' :: [Score] -> Age -> Bool
terminate' scores age = not $ age < 1000

-- invariant: len [SearchSolution] == 1
mutate' :: Seed -> QuerySequence -> Scorer -> [BetaStrand] -> [SearchSolution] -> [SearchSolution]
mutate' seed query scorer betas solutions = [scorer query betas $ mutate'' guesses 0 $ mkStdGen seed] 
  where guesses = snd $ head solutions
        mutate'' :: SearchGuess -> Int -> StdGen -> SearchGuess
        mutate'' (g:gs) i gen = g' : mutate'' gs (i+1) gen'
          where (g', gen') = randomR (lo, hi) gen
                lo = if i == 0 then
                       0
                     else
                       (len $ betas !! (i - 1))
                       + guesses !! (i - 1)
                hi = if i == length guesses then
                       V.length query - (len $ betas !! i)
                     else
                       guesses !! (i + 1)
                       - (len $ betas !! i)

