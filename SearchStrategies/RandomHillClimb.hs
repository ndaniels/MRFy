module SearchStrategies.RandomHillClimb where

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

accept' :: Seed -> [SearchSolution] -> Age -> Bool
accept' _ (s1:s2:solutions) _ = if s1score < s2score then
                                  (s1:s2:solutions)
                                else
                                  (s2:solutions)
  where (s1score, _) = s1
        (s2score, _) = s2

terminate' :: [SearchSolution] -> Age -> Bool
terminate' solutions age = if age < 1000 then False else True

mutate' :: Seed -> QuerySequence -> Scorer -> [BetaStrand] -> [SearchSolution] -> [SearchSolution]
mutate' seed query scorer betas solutions = (scorer query betas $ mutate'' guesses 0 $ mkStdGen seed) : solutions
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
                       length query - (len $ betas !! i)
                     else
                       guesses !! (i + 1)
                       - (len $ betas !! i)

