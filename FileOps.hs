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
import Test.QuickCheck

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
import SearchStrategy (tickProp)
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


runCommand (TestHMM "tickProp") =
  do quickCheck tickProp

runCommand (TestHMM t) =
  error $ "I never heard of test " ++ t

runCommand (AlignmentSearch searchParams
                (files @ Files { hmmPlusF = hmmPlusFile, outputF = outFile })) = do
  (header, hmm, queries) <- loadTestData files
  rgn <- getStdGen
  -- secPred <- getSecondary $ fastaF files
  finish (betas header) hmm queries (randoms rgn)
      where finish bs hmm queries seeds =
              if outFile == "stdout" then
                  mapM_ putStrLn output
              else
                  mapM_ (writeFile outFile) $ intersperse "\n" output
              where strat q = strategy searchParams hmm searchParams q bs
                                          -- XXX why searchParams *twice* ???
                    scorer q = score hmm q bs
                    searchQ r q = search (strat q (scorer q)) (mkStdGen r)
                    trySearch r q = if alignable q bs then searchQ r q else noSearch
                    searches =
                      map trySearch $ take (multiStartPopSize searchParams) seeds

                    results = map (historySolution . popSearch searches) queries
                    output  = [ "Score: " ++ (show $ scoreOf $ head results) 
                              , ""
                              , concat $ intersperse "\n\n" $
                                zipWith (outputAlignment hmm bs) results queries
                              ]



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

noSearch = Aged (Scored [] negLogZero) 0 `hcons` emptyHistory


