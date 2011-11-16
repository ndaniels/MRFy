module StochasticSearch where
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
type Score = Double
type SearchSolution = (Score, SearchGuess)
type Temperature = Double
type Seed = Double
type Age = Int
type Scorer = QuerySequence -> HMM -> [BetaStrand] -> SearchGuess
data SearchStrategy = SearchStrategy { accept :: Seed -> [SearchSolution] -> Age -> Bool
                                     , terminate :: [SearchSolution] -> Age -> Bool
                                     , mutate :: Seed -> Scorer -> [SearchSolution] -> [SearchSolution]
                                     , initialize :: Seed -> [SearchSolution]
                                     }

search :: QuerySequence -> HMM -> [BetaStrand] -> SearchStrategy -> [Seed] -> SearchSolution

search query hmm betas strategy seeds = search' seeds initialGuess 0
  where search' (s1:s2:s3:seeds) guesses age = let solution = mutate' guesses
                                        in if accept' solution age then
                                              if terminate' solution age then
                                                solution
                                               else
                                                 search' seeds solution (age + 1)
                                           else
                                             search' seeds guesses (age + 1)
        initialGuess = initialize strategy $ s1
        mutate' = mutate strategy $ s2 score
        terminate' = terminate strategy
        accept' = accept strategy $ s3

score :: Scorer

score query hmm betas = map (viterbi query) miniHmms
  where miniHmms = []