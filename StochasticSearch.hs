module StochasticSearch where

import qualified Data.Vector as V

import HmmPlus
import Constants
import Viterbi
import Beta
-- this module will take the random number list or generator, and will
-- pass random numbers to the mutating function, possibly the initialization function,
-- and possibly the accepting function

-- scoring function
--  this calls Viterbi (maybe only for later guesses?) and also computes pairwise beta score
-- accepting function
--   takes an old score and new score (and maybe a temperature) and accepts or rejects
-- terminating function (keep in this module?)
-- mutating function
-- initialization function

-- we have a lot of invariants around guesses: 
-- beta strands separated by 4 or more residues
-- beta strands stay in order
-- all beta strands have a position

-- need a representation of a solution

type SearchGuess = [Int] -- list of *starting* residue positions of each beta strand
type SearchSolution = (Score, SearchGuess)
type Temperature = Double
type Seed = Int
type Age = Int
type History = [Score]
type Scorer = QuerySequence -> [BetaStrand] -> SearchGuess -> SearchSolution
data SearchStrategy = SearchStrategy { accept :: Seed -> History -> Age -> Bool
                                     , terminate :: History -> Age -> Bool
                                     , mutate :: Seed -> QuerySequence -> Scorer -> [BetaStrand] -> [SearchSolution] -> [SearchSolution]
                                     , initialize :: Seed -> QuerySequence -> [BetaStrand]-> [SearchGuess]
                                     }

-- invariant: fst SearchSolution == head History
search :: QuerySequence -> HMM -> [BetaStrand] -> SearchStrategy -> [Seed] -> (SearchSolution, History)

search query hmm betas strategy seeds = search' (tail seeds) initialGuess [] 0
  where initialGuess = map (score hmm query betas) $ initialize strategy (head seeds) query betas

        search' :: [Seed] -> [SearchSolution] -> History -> Age -> (SearchSolution, History)
        search' (s1:s2:seeds) guesses hist age =
          let population = mutate' guesses
              score = fst $ minimum population
              newhist = score : hist
            in if accept' newhist age then
                  if terminate' newhist age then
                    (minimum population, newhist)
                   else
                    search' seeds population newhist (age + 1)
               else
                 search' seeds guesses hist (age + 1)
            where mutate' = mutate strategy s1 query (score hmm) betas
                  terminate' = terminate strategy
                  accept' = accept strategy s2

data BetaOrViterbi = Beta
                     | Viterbi

oppAligner Beta = Viterbi
oppAligner Viterbi = Beta

score :: HMM -> Scorer
score hmm query betas guesses = (0.0, [])
  where (miniHmms, hmmAlignTypes) = sliceHmms betas 1 [] []
        miniQueries = undefined

        sliceHmms [] hmmPos hmms atypes = if hmmPos < (V.length hmm) - 1 then
                                            ((V.drop hmmPos hmm) : hmms, Viterbi : atypes)
                                          else
                                            (hmms, atypes)
        sliceHmms (b:b2:bs) hmmPos hmms atypes
          | hmmPos == 1 = sliceHmms betas' initLastPos (initVHmm : initBHmm : hmms) (Viterbi : Beta : atypes)
          | otherwise = sliceHmms betas' lastPos (vHmm : bHmm : hmms) (Viterbi : Beta : atypes)
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b

                initVHmm = V.slice 0 (firstRes b) hmm
                initBHmm = V.slice (firstRes b) (len b) hmm
                initLastPos = firstRes b + len b

                vHmm = V.slice endRes (firstRes b2 - endRes) hmm
                bHmm = V.slice (firstRes b2) (len b2) hmm
                lastPos = firstRes b2 + len b2

                betas' = if hmmPos == 1 then (b:b2:bs) else (b2:bs)

score hmm query betas guesses = (foldr (+) (0.0 :: Double) $ map (fst . viterbi (False, False) Constants.amino query) miniHmms, [])
  where miniHmms = []
  -- really, mutate' should take the query and hmm as arguments, and call score as appropriate
  -- remaining question: if we want to try deferring Viterbi until later generations, how do we
  -- do this? obviously age needs to be a parameter, but maybe we want a variety of scorers?
