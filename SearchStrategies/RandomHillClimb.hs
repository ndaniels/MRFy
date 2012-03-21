module SearchStrategies.RandomHillClimb where

import qualified Data.Vector as V
import System.Random (mkStdGen, randomR, StdGen)

import Debug.Trace (trace)

import Beta
import Constants
import HmmPlus
import qualified Score
import qualified SearchModel as S
import SearchModel (gen0, nextGen, quit)
import SearchStrategy
import StochasticSearch
import Viterbi

ss :: SearchStrategy
ss = SearchStrategy { accept = accept'
                    , terminate = terminate'
                    , mutate = mutate'
                    , initialize = initialize'
                    }
                    
wrap_scorer :: Scorer -> S.Scorer SearchGuess
wrap_scorer = undefined

unwrap_scorer :: S.Scorer SearchGuess -> Scorer
unwrap_scorer = undefined

wrap   :: SearchSolution -> Score.Scored SearchGuess
unwrap :: Score.Scored SearchGuess -> SearchSolution

wrap = undefined
unwrap = undefined

nss :: HMM -> SearchParameters -> QuerySequence -> [BetaStrand] -> S.SearchStrategy SearchGuess
nss hmm searchP query betas =
      S.SS { gen0 = \seed -> initialize' hmm searchP seed query betas 
           , nextGen = \seed scorer solutions -> map wrap $ mutate' searchP seed query (unwrap scorer) betas (map unwrap solutions)
           , S.accept = undefined
           , quit = undefined
           }
initialize' :: HMM -> SearchParameters -> Seed -> QuerySequence -> [BetaStrand] -> [SearchGuess]
initialize' hmm searchP seed query betas = [initialGuess hmm (getSecPreds searchP) seed query betas]

accept' :: SearchParameters -> Seed -> History -> Age -> Bool
accept' _ _ [] _ = error "go away"
accept' _ _ [s1] _ = True
accept' _ _ (s1:s2:scores) _ = fst s1 < fst s2

terminate' :: SearchParameters -> History -> Age -> Bool
terminate' searchP scores age = showMe $ not $ age < (generations searchP)
  where showMe = if not $ (10.0 * ((fromIntegral age) / (fromIntegral (generations searchP)))) `elem` [1.0..10.0] then
                   id
                 else
                   trace ((show age) ++ " generations complete")

-- invariant: len [SearchSolution] == 1
mutate' :: SearchParameters -> Seed -> QuerySequence -> Scorer -> [BetaStrand] -> [SearchSolution] -> [SearchSolution]
mutate' searchP seed query scorer betas solutions = [scorer query betas $ mutate'' guesses 0 (mkStdGen seed) 0]
  where guesses = snd $ head solutions

        mutate'' :: SearchGuess -> Int -> StdGen -> Int -> SearchGuess
        mutate'' [] _ _ _ = []
        mutate'' (g:gs) i gen lastGuess = g' : mutate'' gs (i+1) gen' g'
          where (g', gen') = randomR (lo, hi) gen
                lo = if i == 0 then
                       0
                     else
                       (len $ (betas !! (i - 1))) + lastGuess
                hi = if i == (length guesses) - 1 then
                       V.length query - (len $ betas !! i)
                     else
                       (guesses !! (i + 1)) - (len $ betas !! i)

