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
outputAlignment hmm betas ps querySeq = showAlignment hmm betas querySeq sp 60 Constants.amino
  where sp = statePath hmm querySeq betas ps

popSearch :: [QuerySequence -> (Scored Placement, [Scored Age])]
          -> QuerySequence
          -> (Scored Placement, [Scored Age])
popSearch searches q = minimum $! (parMap rseq) (\s -> let !x = s q in x) searches

newRandoms s = randoms $ mkStdGen s

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

          putStrLn $ show queries

          let strat = \q -> strategy searchParams hmm searchParams q betas
          let scorer = \q -> score hmm q betas
          let searches = map (\r q -> search (strat q) (scorer q) (newRandoms r))
                         $ take (multiStartPopSize searchParams) ((randoms rgn) :: [Seed])

          let results = map (popSearch searches) queries

          putStrLn $ foldr (\s ss -> s ++ "\n\n" ++ ss) "" 
                   $ map (\((ss, hist), query) -> outputAlignment hmm betas ss query) 
                   $ zip results queries 
          putStrLn $ "Score: " ++ (show $ scoreOf $ fst $ head results) 
          putStrLn $ "History: " ++ (show $ snd $ head results) 
          -- putStrLn $ show $ myminimum [results1, results2]

