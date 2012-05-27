module FileOps
       ( Commanded(..)
       , loadTestData, translateQuery
       , runCommand
       )
where
  
import Control.Parallel.Strategies

import Data.Array
import Data.List as DL
import System.Console.CmdArgs
import System.Random (getStdGen, mkStdGen, randoms)
import qualified Data.Vector as V hiding (map)
import System.Environment

import Bio.Sequence
import Bio.Sequence.Fasta

import Beta
-- import HmmAlign
import CommandArgs
import Constants
import HMMPlus
import HMMProps
import MRFTypes
import RunPsiPred
import Score
import SearchModel
import ShowAlignment
import StochasticSearch
import Viterbi

loadTestData :: Files -> IO (HMMHeader, HMM, [QuerySequence])
loadTestData files =
  do querySeqs <- readFasta $ fastaF files
     mrf <- parseMRF $ hmmPlusF files
     return (hmmHeader $ checkMRF mrf, hmm $ checkMRF mrf, map (translateQuery . toStr . seqdata) querySeqs)
     
translateQuery :: String -> V.Vector Int
translateQuery = V.fromList . map lookup
  where lookup k = case V.elemIndex k Constants.amino of
                        Just i -> i
                        Nothing -> error "Residue not found in alphabet"

runCommand :: Commanded -> IO ()
runCommand (TestHmm "mini") =
  do test <- loadTestData $ Files "testing/mini8.hmm+" "testing/mini8.fasta" "/dev/null"
     let ok = oneTestAdmissible test
     putStrLn $ "Function viterbiAdmissible " ++
                (if ok then "passes" else "DOES NOT PASS") ++ " one test"

runCommand (TestHmm "mini-strings") =
  do test <- loadTestData $ Files "testing/mini8.hmm+" "testing/mini8.fasta" "/dev/null"
     mapM_ putStrLn $ oneTestResults test

runCommand (TestHmm "8") =
  do test <- loadTestData $ Files "testing/8.hmm+" "testing/8.fasta" "/dev/null"
     let ok = oneTestAdmissible test
     putStrLn $ "Function viterbiAdmissible " ++
                (if ok then "passes" else "DOES NOT PASS") ++ " one test"

runCommand (TestHmm "8-strings") =
  do test <- loadTestData $ Files "testing/8.hmm+" "testing/8.fasta" "/dev/null"
     mapM_ putStrLn $ oneTestResults test


runCommand (TestHmm t) =
  error $ "I never heard of test " ++ t

runCommand (AlignmentSearch searchParams files) = run
  where hmmPlusFile = hmmPlusF files
        fastaFile = fastaF files
        run = do -- secPred <- getSecondary $ fastaFile
                 (header, hmm, queries) <- loadTestData files
                 let bs = betas header

                 -- putStrLn $ show queries
                 let strat q = strategy searchParams hmm searchParams q bs
                 let scorer q = score hmm q bs

                 let searchQ r q = search (strat q) (scorer q) (newRandoms r)
                 let trySearch r q = if alignable q bs then searchQ r q else noSearch
                 rgn <- getStdGen
                 let searches = map (\r q -> trySearch r q) $
                       take (multiStartPopSize searchParams) ((randoms rgn) :: [Seed])

                 let results = map (popSearch searches) queries

                 mapM_ putStrLn [ "History: " ++ (show $ snd $ head results) 
                                , ""
                                , "Score: " ++ (show $ scoreOf $ fst $ head results) 
                                , ""
                                , concat $ intersperse "\n\n" $
                                  zipWith (\(ss, hist) query ->
                                          outputAlignment hmm bs ss query)
                                          results queries
                                ]



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


