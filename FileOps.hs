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
import qualified Data.Vector.Unboxed as V hiding (map)
import System.Environment

import Bio.Sequence
import Bio.Sequence.Fasta

import Beta

import CommandArgs
import Constants
import HMMPlus
import HMMProps
import LazySearchModel
import MRFTypes
import RunPsiPred
import Score
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
runCommand (TestHMM "mini") =
  do test <- loadTestData $ Files "testing/mini8.hmm+" "testing/mini8.fasta" "/dev/null"
     let ok = oneTestAdmissible test
     putStrLn $ "Function viterbiAdmissible " ++
                (if ok then "passes" else "DOES NOT PASS") ++ " one test"

runCommand (TestHMM "mini-strings") =
  do test <- loadTestData $ Files "testing/mini8.hmm+" "testing/mini8.fasta" "/dev/null"
     mapM_ putStrLn $ oneTestResults test

runCommand (TestHMM "8") =
  do test <- loadTestData $ Files "testing/8.hmm+" "testing/8.fasta" "/dev/null"
     let ok = oneTestAdmissible test
     putStrLn $ "Function viterbiAdmissible " ++
                (if ok then "passes" else "DOES NOT PASS") ++ " one test"

runCommand (TestHMM "8-strings") =
  do test <- loadTestData $ Files "testing/8.hmm+" "testing/8.fasta" "/dev/null"
     mapM_ putStrLn $ oneTestResults test

runCommand (TestHMM "micro8") =
  do test <- loadTestData $ Files "testing/micro8.hmm+" "testing/micro8.fasta" "/dev/null"
     let ok = oneTestAdmissible test
     putStrLn $ "Function viterbiAdmissible " ++
                (if ok then "passes" else "DOES NOT PASS") ++ " one test"

runCommand (TestHMM "micro8-strings") =
  do test <- loadTestData $ Files "testing/micro8.hmm+" "testing/micro8.fasta" "/dev/null"
     mapM_ putStrLn $ oneTestResults test


runCommand (TestHMM t) =
  error $ "I never heard of test " ++ t

runCommand (AlignmentSearch searchParams files) = run
  where hmmPlusFile = hmmPlusF files
        fastaFile = fastaF files
        outFile = outputF files
        run = do -- secPred <- getSecondary $ fastaFile
                 (header, hmm, queries) <- loadTestData files
                 let bs = betas header

                 -- putStrLn $ show queries
                 let strat q = strategy searchParams hmm searchParams q bs
                 let scorer q = score hmm q bs

                 let searchQ r q = search (strat q (scorer q)) (mkStdGen r)
                 let trySearch r q = if alignable q bs then searchQ r q else noSearch
                 rgn <- getStdGen
                 let searches = map trySearch $
                       take (multiStartPopSize searchParams) (randoms rgn)

                 let results = map (popSearch searches) queries

                 let output  =  [ "Score: " ++ (show $ scoreOf $ historySolution $ head results) 
                                , ""
                                , concat $ intersperse "\n\n" $
                                  zipWith (\hist query ->
                                          outputAlignment hmm bs (historySolution hist) query)
                                          results queries
                                ]
                 if outFile == "stdout" then
                      (mapM_ putStrLn output)
                      else
                      (writeFile outFile $ concat $ intersperse "\n" output)



outputAlignment :: HMM -> [BetaStrand] -> Scored Placement -> QuerySequence -> String
outputAlignment hmm betas ps querySeq = 
    if alignable querySeq betas
    then showAlignment hmm betas querySeq sp 60 Constants.amino
    else "Query sequence shorter than combined beta strands; no alignment possible"
  where sp = statePath hmm querySeq betas ps



popSearch :: [QuerySequence -> History Placement]
          -> QuerySequence
          -> History Placement
popSearch searches q = minimum $ (parMap rseq) (\s -> s q) searches

newRandoms s = randoms $ mkStdGen s
noSearch = Aged (Scored [] negLogZero) 0 `hcons` emptyHistory


