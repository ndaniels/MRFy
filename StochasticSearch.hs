module StochasticSearch where

import qualified Data.Vector as V
import qualified Data.List as DL

import Debug.Trace (trace)

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
            in if trace ("age: " ++ (show age) ++ "--- population: " ++ (show guesses)) $ accept' newhist age then
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

vfoldr3 :: (BetaResidue -> HmmNode -> Int -> Score -> Score) -> Score -> [BetaResidue] -> HMM -> QuerySequence -> Score
-- vfoldr3 :: (a -> b -> c -> d -> d) -> d -> [a] -> Vector b -> Vector c -> d 
vfoldr3 f init [] _ _ = init
vfoldr3 f init (r:rs) hmm query = f r (n V.! 0) (q V.! 0) $ vfoldr3 f init rs ns qs
  where (n, ns)  = V.splitAt 1 hmm
        (q, qs) = V.splitAt 1 query

dupeElements [] = []
dupeElements (x:xs) = x : x : (dupeElements xs)

score :: HMM -> Scorer
score hmm query betas guesses = trace (show guesses) $ (foldr (+) 0.0 $ map viterbiOrBeta $ DL.zip4 hmmAlignTypes miniHmms miniQueries $ dupeElements [0..], guesses)
  where viterbiOrBeta :: (BetaOrViterbi, HMM, QuerySequence, Int) -> Score
        viterbiOrBeta (Beta, ns, qs, i) = trace "BETA" $ betaScore (residues ((trace ("6--" ++ (show i)) $ betas !! i))) ns qs
        viterbiOrBeta (Viterbi, ns, qs, i) = trace "VITERBI" $ fst $ viterbi (False, False) Constants.amino query ns

        (miniHmms, hmmAlignTypes) = sliceHmms betas 1 [] []
        miniQueries = sliceQuery betas guesses 1 []

        -- invariant: length residues == length hmmSlice == length querySlice
        betaScore :: [BetaResidue] -> HMM -> QuerySequence -> Score
        betaScore = vfoldr3 betaScore' 0.0

        betaScore' :: BetaResidue -> HmmNode -> Int -> Score -> Score
        betaScore' r n q s = s + betaCoeff * betaTableScore + (1 - betaCoeff) * viterbiScore
          where viterbiScore = transProb + eProb
                eProb = (matchEmissions $ n) V.! q
                transProb = case logProbability $ m_m $ transitions n of
                                 NonZero p -> p
                                 LogZero -> maxProb

                betaTableScore = foldr tableLookup 0.0 $ pairs r
                tableLookup pair score = score + lookupScore
                  where lookupScore = case expose pair of
                                           Buried -> betaBuried V.! partnerInd V.! q
                                           Exposed -> betaExposed V.! partnerInd V.! q
                        partnerInd = query V.! (((trace "7" $ guesses !! (pairStrandSerial pair))) + (residueInd pair) - 1)

        -- invariant: length betas == length guesses
        sliceQuery betas guesses queryPos queries = reverse $ sliceQuery' betas guesses queryPos queries

        sliceQuery' :: [BetaStrand] -> SearchGuess -> Int -> [QuerySequence] -> [QuerySequence]
        sliceQuery' [] [] queryPos queries = if queryPos < (V.length query) - 1 then
                                              (V.drop queryPos query) : queries
                                            else
                                              queries
        sliceQuery' [b] [g] queryPos queries = if length betas /= 1 then
                                             sliceQuery' [] [] queryPos queries
                                           else
                                             sliceQuery' [] [] endRes (bQuery : vQuery : [])
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b
                vQuery = V.slice 0 (firstRes b) query
                bQuery = V.slice (firstRes b) (len b) query
        sliceQuery' (b:b2:bs) (g:g2:gs) queryPos queries
          | queryPos == 1 = sliceQuery' betas' guesses' initLastPos (initBQuery : initVQuery : queries)
          | otherwise = sliceQuery' betas' guesses' lastPos (bQuery : vQuery : queries)
          where endRes = g + len b

                initVQuery = V.slice 0 g query
                initBQuery = V.slice g (len b) query
                initLastPos = g + len b

                vQuery = V.slice endRes (g2 - endRes) query
                bQuery = V.slice g2 (len b2) query
                lastPos = g2 + len b2

                betas' = if queryPos == 1 then (b:b2:bs) else (b2:bs)
                guesses' = if queryPos == 1 then (g:g2:gs) else (g2:gs)

        sliceHmms betas hmmPos hmms atypes = (reverse hmms', reverse atypes')
          where (hmms', atypes') = sliceHmms' betas hmmPos hmms atypes

        sliceHmms' [] hmmPos hmms atypes = if hmmPos < (V.length hmm) - 1 then
                                            ((V.drop hmmPos hmm) : hmms, Viterbi : atypes)
                                          else
                                            (hmms, atypes)
        sliceHmms' [b] hmmPos hmms atypes = if length betas /= 1 then
                                             sliceHmms' [] hmmPos hmms atypes
                                           else
                                             sliceHmms' [] endRes (bHmm : vHmm : []) (Beta : Viterbi : [])
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b
                vHmm = V.slice 0 (firstRes b) hmm
                bHmm = V.slice (firstRes b) (len b) hmm
        sliceHmms' (b:b2:bs) hmmPos hmms atypes
          | hmmPos == 1 = sliceHmms' betas' initLastPos (initBHmm : initVHmm : hmms) (Beta : Viterbi : atypes)
          | otherwise = sliceHmms' betas' lastPos (bHmm : vHmm : hmms) (Beta : Viterbi : atypes)
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b

                initVHmm = V.slice 0 (firstRes b) hmm
                initBHmm = V.slice (firstRes b) (len b) hmm
                initLastPos = firstRes b + len b

                vHmm = V.slice endRes (firstRes b2 - endRes) hmm
                bHmm = V.slice (firstRes b2) (len b2) hmm
                lastPos = firstRes b2 + len b2

                betas' = if hmmPos == 1 then (b:b2:bs) else (b2:bs)

-- score hmm query betas guesses = (foldr (+) (0.0 :: Double) $ map (fst . viterbi (False, False) Constants.amino query) miniHmms, []) 
  -- where miniHmms = [] 
  -- really, mutate' should take the query and hmm as arguments, and call score as appropriate
  -- remaining question: if we want to try deferring Viterbi until later generations, how do we
  -- do this? obviously age needs to be a parameter, but maybe we want a variety of scorers?
