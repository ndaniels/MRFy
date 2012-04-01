{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Main where

import Control.Parallel (par)
import Control.Parallel.Strategies

import Data.Array
import Data.List as DL
import System.Console.CmdArgs
import System.Random (getStdGen, mkStdGen, randoms)
import qualified Data.Vector as V
import System.Environment

import Bio.Sequence
import Bio.Sequence.Fasta

import Beta
-- import HmmAlign
import CommandArgs
import Constants
import HmmPlus
import RunPsiPred
import Score
import SearchModel
import ShowAlignment
import StochasticSearch
import Viterbi


translateQuery :: String -> V.Vector Int
translateQuery = V.fromList . DL.map lookup
  where lookup k = case V.elemIndex k Constants.amino of
                        Just i -> i
                        Nothing -> error "Residue not found in alphabet"

outputAlignment :: HMM -> [BetaStrand] -> Scored Placement -> QuerySequence -> String
outputAlignment hmm betas ps querySeq = 
    if alignable querySeq betas
    then showAlignment hmm betas querySeq sp 60 Constants.amino
    else "Query sequence shorter than combined beta strands; no alignment possible"
  where sp = statePath hmm querySeq betas ps



popSearch :: [QuerySequence -> (Scored Placement, [Scored Age])]
          -> QuerySequence
          -> (Scored Placement, [Scored Age])
popSearch searches q = minimum $ (parMap rseq) (\s -> s q) searches

newRandoms s = randoms $ mkStdGen s
noSearch = (Scored [] negLogZero, [])

main = do argv <- getArgs
          (searchParams, files) <- getOpts argv
          let hmmPlusFile = hmmPlusF files
          let fastaFile = fastaF files
          rgn <- getStdGen
          querySeqs <- readFasta $ fastaFile
          secPred <- getSecondary $ fastaFile

          (header, hmm, md) <- parse $ hmmPlusFile
          let betas = getBetaStrands header
          let queries = map (translateQuery . toStr . seqdata) querySeqs

          -- putStrLn $ show queries
          let strat = \q -> strategy searchParams hmm searchParams q betas
          let scorer = \q -> score hmm q betas


          let searchQ r q = search (strat q) (scorer q) (newRandoms r)
          let trySearch r q = if alignable q betas then searchQ r q else noSearch
          let searches = map (\r q -> trySearch r q)
                         $ take (multiStartPopSize searchParams) ((randoms rgn) :: [Seed])

          let results = map (popSearch searches) queries

          putStrLn $ "History: " ++ (show $ snd $ head results) 
          putStrLn ""
          putStrLn $ "Score: " ++ (show $ scoreOf $ fst $ head results) 
          putStrLn ""
          putStrLn $ foldr (\s ss -> s ++ "\n\n" ++ ss) "" 
                   $ map (\((ss, hist), query) -> outputAlignment hmm betas ss query)
                   $ zip results queries 

