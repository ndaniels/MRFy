module StochasticSearch where

import Control.Parallel (par)
import Control.Parallel.Strategies

import qualified Data.Vector as V
import qualified Data.List as DL

import Debug.Trace (trace)

-- import qualified Wrappers as W 
import Beta
import Constants
import ConstantsGen
import HmmPlus
import PsiPred
import Score
import SearchModel
import Viterbi
import Wrappers

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

data SearchParameters = SearchParameters { strategy :: SearchStrategy Placement
                                             , generations :: Int
                                             , multiStartPopSize :: Int
                                             , verbose :: Bool
                                             , populationSize :: Maybe Int
                                             , initialTemperature :: Maybe Double
                                             , coolingFactor :: Maybe Double
                                             , boltzmannConstant :: Maybe Double
                                             , mutationRate :: Maybe Double
                                             , secPreds :: Maybe [SSPrediction]
                                             }

getSearchParm searchP parm = maybe (error "Not a valid parameter.") id (parm searchP)

getSecPreds :: SearchParameters -> [SSPrediction]
getSecPreds searchP = case secPreds searchP of
                        Just preds -> preds
                        Nothing -> []

type Placement = [Int] -- list of *starting* residue positions of each beta strand

type Temperature = Double

-- type Scorer = QuerySequence -> [BetaStrand] -> SearchGuess -> SearchSolution
-- data SearchStrategy = SearchStrategy { accept :: SearchParameters -> Seed -> History -> Age -> Bool
--                                      , terminate :: SearchParameters -> History -> Age -> Bool
--                                      , mutate :: SearchParameters -> Seed -> QuerySequence -> Scorer -> [BetaStrand] -> [SearchSolution] -> [SearchSolution]
--                                      , initialize :: HMM -> SearchParameters -> Seed -> QuerySequence -> [BetaStrand]-> [SearchGuess]
--                                      }


-- invariant: fst SearchSolution == head History
-- search :: QuerySequence -> HMM -> [BetaStrand] -> SearchParameters -> [Seed] -> (SearchSolution, History)
-- search query hmm betas searchP seeds = search' (tail seeds) initialGuessScore [] 0
--   where initialGuessScore = map (score hmm query betas) initialGuess
-- 
--         initialGuess = (initialize strat) hmm searchP (head seeds) query betas
-- 
--         strat = strategy searchP
--         search' :: [Seed] -> [SearchSolution] -> History -> Age -> (SearchSolution, History)
--         search' (s1:s2:seeds) oldPop hist age =
--           let newPop = mutate' oldPop
--               score = fst $ minimum newPop
--               -- score = trace (show initialGuess) $ fst $ minimum newPop 
--               -- score = trace (show $ DL.sort $ map fst newPop) $ fst $ minimum newPop 
--               -- newHist = trace ("New score: " DL.++ (show score) DL.++ "--- History: " DL.++ (show hist)) $ score : hist 
--               newHist = (score, age) : hist
--             in if accept' newHist age then
--                   if terminate' newHist age then
--                     (minimum newPop, newHist)
--                   else
--                     search' seeds newPop newHist (age + 1)
--                else
--                   if terminate' hist age then
--                     (minimum oldPop, hist)
--                   else
--                     search' seeds oldPop hist (age + 1)
--             where mutate' = mutate strat searchP s1 query (score hmm) betas
--                   terminate' = terminate strat searchP
--                   accept' = accept strat searchP s2

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

statePath :: HMM -> QuerySequence -> [BetaStrand] -> Scored Placement -> StatePath
statePath hmm query betas ps = foldr (++) [] $ map viterbiOrBeta $ DL.zip4 hmmAlignTypes (map traceid miniHmms) miniQueries $ dupeElements [0..]
  where viterbiOrBeta :: (BetaOrViterbi, HMM, QuerySequence, Int) -> StatePath
        viterbiOrBeta (Beta, ns, qs, i) = take (len (betas !! i)) $ repeat BMat
        viterbiOrBeta (Viterbi, ns, qs, i) = unScored $ viterbi consPath (False, False) Constants.amino qs ns

        -- traceid hmm = trace (show (V.map nodeNum hmm)) $ id hmm 
        -- traceid = (trace (show guesses)) id 
        traceid = id

        (miniHmms, hmmAlignTypes) = sliceHmms hmm betas 1 [] []
        miniQueries = sliceQuery query betas (unScored ps) 1 []

score :: HMM -> QuerySequence -> [BetaStrand] -> Scorer Placement
score hmm query betas ps = Scored ps (foldr (+) negLogOne $ (parMap rseq) viterbiOrBeta $ DL.zip4 hmmAlignTypes (map traceid miniHmms) miniQueries $ dupeElements [0..])
  where viterbiOrBeta :: (BetaOrViterbi, HMM, QuerySequence, Int) -> Score
        viterbiOrBeta (Beta, ns, qs, i) = betaScore query ps (residues (betas !! i)) ns qs
        viterbiOrBeta (Viterbi, ns, qs, i) = scoreOf $ viterbi consNoPath (False, False) Constants.amino qs ns

        -- traceid hmm = trace (show (V.map nodeNum hmm)) $ id hmm 
        traceid = id
        -- traceid = (trace (show hmmAlignTypes)) id 

        (miniHmms, hmmAlignTypes) = sliceHmms hmm betas 1 [] []
        miniQueries = sliceQuery query betas ps 1 []

-- invariant: length residues == length hmmSlice == length querySlice
betaScore :: QuerySequence -> Placement -> [BetaResidue] -> HMM -> QuerySequence -> Score
betaScore query guesses = vfoldr3 betaScore' negLogOne
  where betaScore' :: BetaResidue -> HmmNode -> Int -> Score -> Score
        betaScore' r n q s = s + Score (betaCoeff * unScore betaTableScore) + Score ((1 - betaCoeff) * unScore viterbiScore)
          where viterbiScore = transProb + eProb -- replace with transScore and emissionScore
                                                 -- also, not blindly m_m
                                                 -- do we have to look at neighboring states?
                                                 -- e.g. came from i or d rather than m?
                                                 -- do we need a prevState, usually m?
                eProb = (matchEmissions $ n) V.! q
                transProb = case logProbability $ m_m $ transitions n of
                                 NonZero p -> p
                                 LogZero -> maxProb
        
                betaTableScore = foldr tableLookup 0.0 $ pairs r
                tableLookup pair score = score + lookupScore
                  where lookupScore = case expose pair of
                                           Buried -> betaBuried V.! partnerInd V.! q
                                           Exposed -> betaExposed V.! partnerInd V.! q
                        partnerInd = query V.! partnerBeta
                        partnerBeta = (guesses !! (pairStrandSerial pair)) + (residueInd pair)

-- invariant: length betas == length guesses
sliceQuery :: QuerySequence -> [BetaStrand] -> Placement -> Int -> [QuerySequence] -> [QuerySequence]
sliceQuery query betas placement queryPos queries = reverse $ sliceQuery' betas placement queryPos queries
  where sliceQuery' :: [BetaStrand] -> Placement -> Int -> [QuerySequence] -> [QuerySequence]
        sliceQuery' [] [] queryPos queries = (V.drop queryPos query) : queries
        sliceQuery' [b] [g] queryPos queries = if length betas /= 1 then
                                             sliceQuery' [] [] queryPos queries
                                           else
                                             sliceQuery' [] [] endRes (bQuery : vQuery : [])
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b
                vQuery = vslice "1" 0 (firstRes b) query
                bQuery = vslice "2" (firstRes b) (len b) query
        sliceQuery' (b:b2:bs) (g:g2:gs) queryPos queries
          | queryPos == 1 = sliceQuery' betas' guesses' initLastPos (initBQuery : initVQuery : queries)
          | otherwise = sliceQuery' betas' guesses' lastPos (bQuery : vQuery : queries)
          where endRes = g + len b
        
                initVQuery = vslice "3" 0 g query
                initBQuery = vslice "4" g (len b) query
                initLastPos = g + len b
        
                vQuery = vslice "5" endRes (g2 - endRes) query
                bQuery = vslice "6" g2 (len b2) query
                lastPos = g2 + len b2
        
                betas' = if queryPos == 1 then (b:b2:bs) else (b2:bs)
                guesses' = if queryPos == 1 then (g:g2:gs) else (g2:gs)

sliceHmms hmm betas hmmPos hmms atypes = (reverse hmms', reverse atypes')
  where (hmms', atypes') = sliceHmms' betas hmmPos hmms atypes

        sliceHmms' [] hmmPos hmms atypes = ((V.drop (hmmPos - 1) hmm) : hmms, Viterbi : atypes)
        sliceHmms' [b] hmmPos hmms atypes = if length betas /= 1 then
                                             sliceHmms' [] hmmPos hmms atypes
                                           else
                                             sliceHmms' [] endRes (bHmm : vHmm : []) (Beta : Viterbi : [])
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b
                vHmm = vslice "7" 0 (firstRes b) hmm
                bHmm = vslice "8" (firstRes b) (len b) hmm
        sliceHmms' (b:b2:bs) hmmPos hmms atypes
          | hmmPos == 1 = sliceHmms' betas' initLastPos (initBHmm : initVHmm : hmms) (Beta : Viterbi : atypes)
          | otherwise = sliceHmms' betas' lastPos (bHmm : vHmm : hmms) (Beta : Viterbi : atypes)
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b
        
                initVHmm = vslice "9" 0 (firstRes b) hmm
                initBHmm = vslice "10" (firstRes b) (len b) hmm
                initLastPos = firstRes b + len b
        
                -- the zeroth node should be the LAST BETA node from the previous slice
                -- vHmm = trace ("firstRes: " ++ (show (firstRes b2)) ++ " endRes: " ++ (show endRes)) $ vslice "11" (endRes - 1) (firstRes b2 - endRes + 1) hmm 
                vHmm = vslice "11" (endRes - 1) (firstRes b2 - endRes + 1) hmm
                bHmm = vslice "12" (firstRes b2) (len b2) hmm
                lastPos = firstRes b2 + len b2
        
                betas' = if hmmPos == 1 then (b:b2:bs) else (b2:bs)

