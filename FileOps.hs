module FileOps
       ( Commanded(..)
       , loadTestData, translateQuery
       , runCommand
       , outputAlignment
       )
where
  
import Control.Monad.LazyRandom
import Control.Parallel.Strategies
import ParRandom
import Data.Array
import Data.List as DL
import System.Console.CmdArgs
import System.Random (getStdGen, mkStdGen, randoms)
import qualified Data.Vector.Unboxed as V hiding (map)
import System.Environment
import Test.QuickCheck

import Bio.Sequence.Fasta

import Beta

import CommandArgs
import Constants
import HMMArby
import HMMPlus
import HMMProps
import HyperTriangles
import LazySearchModel
import MRFTypes
import Perturb
import RunPsiPred
import Score
import SearchStrategy (tickProp)
import ShowAlignment
import StochasticSearch
import Viterbi
import V2 (Tree(..), costTree)

loadTestData :: Files -> IO (HMMHeader, HMM, [QuerySequence])
loadTestData files =
  do querySeqs <- readFasta $ fastaF files
     mrf <- parseMRF $ hmmPlusF files
     return (hmmHeader $ checkMRF mrf, hmm $ checkMRF mrf, 
                         map (qSeq . loadQuery) querySeqs)

loadRealData :: Files -> IO (HMMHeader, HMM, [Query])
loadRealData files =
  do querySeqs <- readFasta $ fastaF files
     mrf <- parseMRF $ hmmPlusF files
     return (hmmHeader $ checkMRF mrf, hmm $ checkMRF mrf, map loadQuery querySeqs)
     
runCommand :: Commanded -> IO ()
runCommand (TestHMM "mini") =
  do test <- loadTestData $ Files "testing/mini8.hmm+" "testing/mini8.fasta" "/dev/null"
     res <- quickCheckResult (oneTestAdmissible test)
     putStrLn $ "Function viterbiAdmissible " ++
                (case res of { Success {} -> "passes"; _ -> "DOES NOT PASS" }) ++
                " test mini8"

runCommand (TestHMM "mini-strings") =
  do test <- loadTestData $ Files "testing/mini8.hmm+" "testing/mini8.fasta" "/dev/null"
     mapM_ putStrLn $ oneTestResults test

runCommand (TestHMM "8") =
  do test <- loadTestData $ Files "testing/8.hmm+" "testing/8.fasta" "/dev/null"
     res <- quickCheckResult (oneTestAdmissible test)
     putStrLn $ "Function viterbiAdmissible " ++
                (case res of { Success {} -> "passes"; _ -> "DOES NOT PASS" }) ++
                " test 8"

runCommand (TestHMM "8-strings") =
  do test <- loadTestData $ Files "testing/8.hmm+" "testing/8.fasta" "/dev/null"
     mapM_ putStrLn $ oneTestResults test

runCommand (TestHMM "micro8") =
  do test <- loadTestData $ Files "testing/micro8.hmm+" "testing/micro8.fasta" "/dev/null"
     res <- quickCheckResult (oneTestAdmissible test)
     putStrLn $ "Function viterbiAdmissible " ++
                (case res of { Success {} -> "passes"; _ -> "DOES NOT PASS" }) ++
                " test micro8"

runCommand (TestHMM "micro8-strings") =
  do test <- loadTestData $ Files "testing/micro8.hmm+" "testing/micro8.fasta" "/dev/null"
     mapM_ putStrLn $ oneTestResults test

runCommand (TestHMM "all-perturb-8") =
  do test <- loadTestData $ Files "testing/8.hmm+" "testing/8.fasta" "/dev/null"
     putStrLn $ oneAllMoversPerturb test

runCommand (TestHMM "all-perturb-micro8") =
  do test <- loadTestData $ Files "testing/micro8.hmm+" "testing/micro8.fasta" "/dev/null"
     putStrLn $ oneAllMoversPerturb test

runCommand (TestHMM "decay-perturb-8") =
  do test <- loadTestData $ Files "testing/8.hmm+" "testing/8.fasta" "/dev/null"
     putStrLn $ oneDecayMoversPerturb test

runCommand (TestHMM "decay-perturb-micro8") =
  do test <- loadTestData $ Files "testing/micro8.hmm+" "testing/micro8.fasta" "/dev/null"
     putStrLn $ oneDecayMoversPerturb test

runCommand (TestHMM "local-perturb-8") =
  do test <- loadTestData $ Files "testing/8.hmm+" "testing/8.fasta" "/dev/null"
     putStrLn $ oneLocalPerturb test

runCommand (TestHMM "local-perturb-micro8") =
  do test <- loadTestData $ Files "testing/micro8.hmm+" "testing/micro8.fasta" "/dev/null"
     putStrLn $ oneLocalPerturb test

runCommand (TestHMM "plan7GenProp") =
  quickCheck isPlan7Prop

runCommand (TestHMM "tickProp") =
  quickCheck tickProp

runCommand (TestHMM "ubProp") =
  quickCheck ubProp

runCommand (TestHMM "buProp") =
  quickCheck buProp

runCommand (TestHMM "blockNoMergeProp") =
  quickCheck blockNoMergeProp

runCommand (TestHMM "mergeMergeProp") =
  quickCheck mergeMergeProp

runCommand (TestHMM "countEnumLaw") =
  quickCheck countEnumLaw

runCommand (TestHMM "pointsWidthLaw") =
  quickCheck pointsWidthLaw

runCommand (TestHMM "twoCountsLaw") =
  quickCheck twoCountsLaw

runCommand (TestHMM "freqLaw") =
  quickCheck freqLaw

runCommand (TestHMM "isEnumLaw") =
  quickCheck isEnumLaw

runCommand (TestHMM "randomLaw") =
  quickCheck randomLaw

runCommand (TestHMM "consistent-scoring") =
  quickCheck consistentScoring

runCommand (TestHMM "scoreable-metrics") =
  quickCheck scoreableMetrics

runCommand (TestHMM "viterbi-awesome") =
  quickCheck viterbiIsAwesome

runCommand (TestHMM "all-perturb") =
  mapM_ run $ perturbProps
    where run (s, p) = do { putStrLn ("Testing " ++ s); quickCheck p }

runCommand (TestHMM "all-props") =
  mapM_ run $ perturbProps ++ hmmProps ++ hyperProps
    where run (s, p) = do { putStrLn ("Testing " ++ s); quickCheck p }

runCommand (TestHMM t) =
  error $ "I never heard of test " ++ t

runCommand (AlignmentSearch searchParams
                (files @ Files { hmmPlusF = hmmPlusFile, outputF = outFile })) = do
  (header, hmm, queries) <- loadRealData files
  rgn <- getStdGen
  -- secPred <- getSecondary $ fastaF files
  finish (betas header) hmm queries (randoms rgn)
      where finish bs hmm queries seeds =
              if outFile == "stdout" then
                  mapM_ putStrLn output
              else
                  writeFile outFile (concat $ intersperse "\n" output)
              where popSize  = multiStartPopSize searchParams
                    searches = take popSize $ map trySearch seeds
                    trySearch r q = if alignable q bs then searchQ else noSearch
                      where searchQ = evalRand search (mkStdGen r)
                            search  = fullSearch (strat (score hmm q bs))
                            strat   = strategy searchParams hmm searchParams q bs                  
                    results = map (historySolution . popSearch searches) qSeqs
                    qSeqs = map qSeq queries
                    output  = [ qHeader $ head queries
                              , "Score: " ++ (show $ scoreOf $ head results) 
                              , ""
                              , concat $ intersperse "\n\n" $
                                zipWith (outputAlignment hmm bs) results queries
                              ]



outputAlignment :: HMM -> [BetaStrand] -> Scored Placement -> Query -> String
outputAlignment hmm betas ps query = 
    if alignable querySeq betas
    then showAlignment hmm betas querySeq sp 60 Constants.amino
    else "Query sequence shorter than combined beta strands; no alignment possible"
  where sp = statePath hmm querySeq betas ps
        querySeq = qSeq query
        h = qHeader query



popSearch :: [QuerySequence -> History Placement]
          -> QuerySequence
          -> History Placement
popSearch searches q = minimum $ (parMap rseq) (\s -> s q) searches

noSearch = CCosted (Scored [] negLogZero) 0 `hcons` emptyHistory


