{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, ExistentialQuantification, RankNTypes #-}
module StochasticSearch where

import Control.Parallel.Strategies

import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.List as DL
import System.Random (StdGen)

-- import qualified Wrappers as W 
import Beta
import Constants
import ConstantsGen
import LazySearchModel ( FullSearchStrategy )
import MRFTypes
import PsiPred
import Score
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

type Scorer placement = placement -> Scored placement

type NewSS
 = HMM -> SearchParameters -> QuerySequence -> [BetaStrand] -> Scorer Placement
 -> FullSearchStrategy Placement StdGen
 
data SearchParameters = SearchParameters { strategy :: NewSS
                                         , generations :: Int
                                         , multiStartPopSize :: Int
                                         , verbose :: Bool
                                         , populationSize :: Maybe Int
                                         , initialTemperature :: Maybe Double
                                         , coolingFactor :: Maybe Double
                                         , boltzmannConstant :: Maybe Double
                                         , mutationRate :: Maybe Double
                                         , convergenceAge :: Maybe Int
                                         , secPreds :: Maybe [SSPrediction]
                                         }

getSearchParm :: SearchParameters -> (SearchParameters ->  Maybe a) -> a
getSearchParm searchP parm = fromMaybe (error "Not a valid parameter.") (parm searchP)

getSecPreds :: SearchParameters -> [SSPrediction]
getSecPreds searchP = case secPreds searchP of
                        Just preds -> preds
                        Nothing -> []

type Placement = [Int] -- list of *starting* residue positions of each beta strand

type Temperature = Double

data BetaOrViterbi = Beta
                     | Viterbi
                     deriving Show

oppAligner :: BetaOrViterbi -> BetaOrViterbi
oppAligner Beta = Viterbi
oppAligner Viterbi = Beta

alignable :: QuerySequence -> [BetaStrand] -> Bool
alignable q bs = bLen < qLen
  where bLen = foldr (+) 0 $ map len bs
        qLen = U.length q


vfoldr3 :: (BetaResidue -> HMMNode -> Int -> Score -> Score) -> Score -> [BetaResidue] -> HMM -> QuerySequence -> Score
-- vfoldr3 :: (a -> b -> c -> d -> d) -> d -> [a] -> Vector b -> Vector c -> d 
vfoldr3 _ init [] _ _ = init
vfoldr3 f init (r:rs) hmm query = f r (n V.! 0) (q U.! 0) $ vfoldr3 f init rs ns qs
  where (n, ns)  = V.splitAt 1 hmm
        (q, qs) = U.splitAt 1 query

dupeElements :: [a] -> [a]
dupeElements [] = []
dupeElements (x:xs) = x : x : dupeElements xs

statePath :: HMM -> QuerySequence -> [BetaStrand] -> Scored Placement -> StatePath
statePath hmm query betas ps = foldr (++) [] $ map viterbiOrBeta $ DL.zip4 hmmAlignTypes (map traceid miniHMMs) miniQueries $ dupeElements [0..]
  where viterbiOrBeta :: (BetaOrViterbi, HMM, QuerySequence, Int) -> StatePath
        viterbiOrBeta (Beta,   _ns, _qs, i) = take (len (betas !! i)) $ repeat BMat
        viterbiOrBeta (Viterbi, ns, qs, _i) =
          unScored $ viterbi consPath (False, False) qs ns

        -- traceid hmm = trace (show (V.map nodeNum hmm)) $ id hmm 
        -- traceid = (trace (show guesses)) id 
        traceid = id

        (miniHMMs, hmmAlignTypes) = sliceHMMs hmm betas 1 [] []
        miniQueries = sliceQuery query betas (unScored ps) 1 []

score :: HMM -> QuerySequence -> [BetaStrand] -> Scorer Placement
score hmm query betas ps = Scored ps (foldr (+) negLogOne $ (parMap rseq) viterbiOrBeta $ DL.zip4 hmmAlignTypes (map traceid miniHMMs) miniQueries $ dupeElements [0..])
  where viterbiOrBeta :: (BetaOrViterbi, HMM, QuerySequence, Int) -> Score
        -- NMD note: I think we'll have to do something with seq here
        -- to force evaluation of the Viterbis before the Betas (Vs can still be in parallel)
        -- so we can use the last node of a Viterbi segment to inform the transition
        -- to Match for the Beta score.
        viterbiOrBeta (Beta, ns, qs, i) = betaScore query ps (residues (betas !! i)) ns qs
        viterbiOrBeta (Viterbi, ns, qs, _) = scoreOf $ viterbi consNoPath (False, False) qs ns

        -- traceid hmm = trace (show (V.map nodeNum hmm)) $ id hmm 
        traceid = id
        -- traceid = (trace (show hmmAlignTypes)) id 

        (miniHMMs, hmmAlignTypes) = sliceHMMs hmm betas 1 [] []
        miniQueries = sliceQuery query betas ps 1 []

-- invariant: length residues == length hmmSlice == length querySlice
betaScore :: QuerySequence -> Placement -> [BetaResidue] -> HMM -> QuerySequence -> Score
betaScore query guesses = vfoldr3 betaScore' negLogOne
  where betaScore' :: BetaResidue -> HMMNode -> Int -> Score -> Score
        betaScore' r n q s = s + Score (betaCoeff * unScore betaTableScore) + Score ((1 - betaCoeff) * unScore viterbiScore)
          where viterbiScore = transScoreNode n Mat Mat + emissionScoreNode n q Mat
                -- also, not blindly m_m
                -- do we have to look at neighboring states?
                -- e.g. came from i or d rather than m?
                -- do we need a prevState, usually m?
                -- We're punting on this bug for now and forcing
                -- a m_m transition.
                betaTableScore = foldr tableLookup negLogOne $ pairs r
                tableLookup pair score = score + lookupScore
                  where lookupScore = case expose pair of
                                           Buried -> betaBuried V.! partnerInd U.! q
                                           Exposed -> betaExposed V.! partnerInd U.! q
                        partnerInd = query U.! partnerBeta
                        partnerBeta = (guesses !! (pairStrandSerial pair)) + (residueInd pair)

-- invariant: length betas == length guesses
sliceQuery :: QuerySequence -> [BetaStrand] -> Placement -> Int -> [QuerySequence] -> [QuerySequence]
sliceQuery query betas placement queryPos queries = reverse $ sliceQuery' betas placement queryPos queries
  where sliceQuery' :: [BetaStrand] -> Placement -> Int -> [QuerySequence] -> [QuerySequence]
        sliceQuery' [] [] queryPos queries = U.drop queryPos query : queries
        sliceQuery' [b] [g] queryPos queries = if length betas /= 1 then
                                             sliceQuery' [] [] queryPos queries
                                           else
                                             sliceQuery' [] [] endRes (bQuery : vQuery : [])
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b
                vQuery = uslice "1" 0 (firstRes b) query
                bQuery = uslice "2" (firstRes b) (len b) query
        sliceQuery' (b:b2:bs) (g:g2:gs) queryPos queries
          | queryPos == 1 = sliceQuery' betas' guesses' initLastPos (initBQuery : initVQuery : queries)
          | otherwise = sliceQuery' betas' guesses' lastPos (bQuery : vQuery : queries)
          where endRes = g + len b
        
                initVQuery = uslice "3" 0 g query
                initBQuery = uslice "4" g (len b) query
                initLastPos = g + len b
        
                vQuery = uslice "5" endRes (g2 - endRes) query
                bQuery = uslice "6" g2 (len b2) query
                lastPos = g2 + len b2
        
                betas' = if queryPos == 1 then (b:b2:bs) else (b2:bs)
                guesses' = if queryPos == 1 then (g:g2:gs) else (g2:gs)
        sliceQuery' [] (_:_) _ _ = error "sliceQuery precondition violated"
        sliceQuery' (_:_) [] _ _ = error "sliceQuery precondition violated"

sliceHMMs hmm betas hmmPos hmms atypes = (reverse hmms', reverse atypes')
  where (hmms', atypes') = sliceHMMs' betas hmmPos hmms atypes
  -- TODO: alternating beta and viterbi can be simplified with 'intersperse' from prelude

        sliceHMMs' [] hmmPos hmms atypes = ((V.drop (hmmPos - 1) hmm) : hmms, Viterbi : atypes)
        sliceHMMs' [b] hmmPos hmms atypes = if length betas /= 1 then
                                             sliceHMMs' [] hmmPos hmms atypes
                                           else
                                             sliceHMMs' [] endRes (bHMM : vHMM : []) (Beta : Viterbi : [])
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b
                vHMM = vslice "7" 0 (firstRes b) hmm
                bHMM = vslice "8" (firstRes b) (len b) hmm
        sliceHMMs' (b:b2:bs) hmmPos hmms atypes
          | hmmPos == 1 = sliceHMMs' betas' initLastPos (initBHMM : initVHMM : hmms) (Beta : Viterbi : atypes)
          | otherwise = sliceHMMs' betas' lastPos (bHMM : vHMM : hmms) (Beta : Viterbi : atypes)
          where firstRes = resPosition . head . residues
                endRes = firstRes b + len b
        
                initVHMM = vslice "9" 0 (firstRes b) hmm
                initBHMM = vslice "10" (firstRes b) (len b) hmm
                initLastPos = firstRes b + len b
        
                -- the zeroth node should be the LAST BETA node from the previous slice
                -- vHMM = trace ("firstRes: " ++ (show (firstRes b2)) ++ " endRes: " ++ (show endRes)) $ vslice "11" (endRes - 1) (firstRes b2 - endRes + 1) hmm 
                vHMM = vslice "11" (endRes - 1) (firstRes b2 - endRes + 1) hmm
                bHMM = vslice "12" (firstRes b2) (len b2) hmm
                lastPos = firstRes b2 + len b2
        
                betas' = if hmmPos == 1 then (b:b2:bs) else (b2:bs)

-- TODO: We currently allow beta strands to abut. This is fine for unpaired strands
-- (which we may have split due to non-consensus residues). But it's not possible
-- for paired strands. We should consider a further restriction on placements
-- that requires at least 3 or 4 residues between strands *iff* those strands
-- contain paired residues.