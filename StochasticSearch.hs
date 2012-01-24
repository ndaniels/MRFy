module StochasticSearch where

import Control.Parallel (par)
import Control.Parallel.Strategies

import qualified Data.Vector as V
import qualified Data.List as DL

import Debug.Trace (trace)

import qualified Wrappers as W

import HmmPlus
import Constants
import ConstantsGen
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
search query hmm betas strategy seeds = search' (tail seeds) initialGuessScore [] 0
  where initialGuessScore = map (score hmm query betas) initialGuess
        initialGuess = initialize strategy (head seeds) query betas

        search' :: [Seed] -> [SearchSolution] -> History -> Age -> (SearchSolution, History)
        search' (s1:s2:seeds) oldPop hist age =
          let newPop = mutate' oldPop
              score = fst $ minimum newPop
              newHist = score : hist
            in if accept' newHist age then
                  if terminate' newHist age then
                    (minimum newPop, newHist)
                  else
                    search' seeds newPop newHist (age + 1)
               else
                 if terminate' hist age then
                   (minimum oldPop, hist)
                 else
                   search' seeds oldPop hist (age + 1)
            where mutate' = mutate strategy s1 query (score hmm) betas
                  terminate' = terminate strategy
                  accept' = accept strategy s2

data BetaOrViterbi = Beta
                     | Viterbi
                     deriving Show

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

statePath :: HMM -> QuerySequence -> [BetaStrand] -> SearchSolution -> StatePath
statePath hmm query betas (_, guesses) = foldr (++) [] $ map viterbiOrBeta $ DL.zip4 hmmAlignTypes (map traceid miniHmms) miniQueries $ dupeElements [0..]
  where viterbiOrBeta :: (BetaOrViterbi, HMM, QuerySequence, Int) -> StatePath
        viterbiOrBeta (Beta, ns, qs, i) = take (len (betas !! i)) $ repeat bmat
        viterbiOrBeta (Viterbi, ns, qs, i) = snd $ viterbi (False, False) Constants.amino qs ns

        -- traceid hmm = trace (show (V.map nodeNum hmm)) $ id hmm 
        -- traceid = (trace (show guesses)) id 
        traceid = id

        (miniHmms, hmmAlignTypes) = sliceHmms hmm betas 1 [] []
        miniQueries = sliceQuery query betas guesses 1 []

score :: HMM -> Scorer
score hmm query betas guesses = (foldr (+) 0.0 $ (parMap rseq) viterbiOrBeta $ DL.zip4 hmmAlignTypes (map traceid miniHmms) miniQueries $ dupeElements [0..], guesses)
  where viterbiOrBeta :: (BetaOrViterbi, HMM, QuerySequence, Int) -> Score
        viterbiOrBeta (Beta, ns, qs, i) = betaScore query guesses (residues (betas !! i)) ns qs
        viterbiOrBeta (Viterbi, ns, qs, i) = viterbiF (False, False) Constants.amino qs ns

        -- traceid hmm = trace (show (V.map nodeNum hmm)) $ id hmm 
        traceid = id
        -- traceid = (trace (show hmmAlignTypes)) id 

        (miniHmms, hmmAlignTypes) = sliceHmms hmm betas 1 [] []
        miniQueries = sliceQuery query betas guesses 1 []

-- invariant: length residues == length hmmSlice == length querySlice
betaScore :: QuerySequence -> SearchGuess -> [BetaResidue] -> HMM -> QuerySequence -> Score
betaScore query guesses = {-# SCC "betaScore" #-} vfoldr3 betaScore' 0.0
  where betaScore' :: BetaResidue -> HmmNode -> Int -> Score -> Score
        betaScore' r n q s = {-# SCC "betaScore'" #-} s + betaCoeff * betaTableScore + (1 - betaCoeff) * viterbiScore
          where viterbiScore = {-# SCC "viterbiScore" #-}  transProb + eProb
                eProb = {-# SCC "eProbStochastic" #-} (matchEmissions $ n) V.! q
                transProb = {-# SCC "transProbStochastic" #-} case logProbability $ m_m $ transitions n of
                                 NonZero p -> p
                                 LogZero -> maxProb
        
                betaTableScore = {-# SCC "betaTableScore" #-} foldr tableLookup 0.0 $ pairs r
                tableLookup pair score = {-# SCC "tableLookup" #-} score + lookupScore
                  where lookupScore = {-# SCC "lookupScore" #-} case expose pair of
                                           Buried -> betaBuried V.! partnerInd V.! q
                                           Exposed -> betaExposed V.! partnerInd V.! q
                        partnerInd = {-# SCC "partnerInd" #-} query V.! partnerBeta
                        partnerBeta = (guesses !! (pairStrandSerial pair)) + (residueInd pair)

-- invariant: length betas == length guesses
sliceQuery query betas guesses queryPos queries = {-# SCC "sliceQuery" #-} reverse $ sliceQuery' betas guesses queryPos queries
  where sliceQuery' :: [BetaStrand] -> SearchGuess -> Int -> [QuerySequence] -> [QuerySequence]
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
                vQuery = W.slice "1" 0 (firstRes b) query
                bQuery = W.slice "2" (firstRes b) (len b) query
        sliceQuery' (b:b2:bs) (g:g2:gs) queryPos queries
          | queryPos == 1 = sliceQuery' betas' guesses' initLastPos (initBQuery : initVQuery : queries)
          | otherwise = sliceQuery' betas' guesses' lastPos (bQuery : vQuery : queries)
          where endRes = g + len b
        
                initVQuery = W.slice "3" 0 g query
                initBQuery = W.slice "4" g (len b) query
                initLastPos = g + len b
        
                vQuery = W.slice "5" endRes (g2 - endRes) query
                bQuery = W.slice "6" g2 (len b2) query
                lastPos = g2 + len b2
        
                betas' = if queryPos == 1 then (b:b2:bs) else (b2:bs)
                guesses' = if queryPos == 1 then (g:g2:gs) else (g2:gs)

sliceHmms hmm betas hmmPos hmms atypes = (reverse hmms', reverse atypes')
  where (hmms', atypes') = {-# SCC "sliceHmms" #-} sliceHmms' betas hmmPos hmms atypes

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
                vHmm = W.slice "7" 0 (firstRes b) hmm
                bHmm = W.slice "8" (firstRes b) (len b) hmm
        sliceHmms' (b:b2:bs) hmmPos hmms atypes
          | hmmPos == 1 = sliceHmms' betas' initLastPos (initBHmm : initVHmm : hmms) (Beta : Viterbi : atypes)
          | otherwise = sliceHmms' betas' lastPos (bHmm : vHmm : hmms) (Beta : Viterbi : atypes)
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b
        
                initVHmm = W.slice "9" 0 (firstRes b) hmm
                initBHmm = W.slice "10" (firstRes b) (len b) hmm
                initLastPos = firstRes b + len b
        
                vHmm = W.slice "11" endRes (firstRes b2 - endRes) hmm
                bHmm = W.slice "12" (firstRes b2) (len b2) hmm
                lastPos = firstRes b2 + len b2
        
                betas' = if hmmPos == 1 then (b:b2:bs) else (b2:bs)

