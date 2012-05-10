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
import HmmPlus
import RunPsiPred
import Score
import SearchModel
import ShowAlignment
import StochasticSearch
import Viterbi

loadTestData :: Files -> IO (SmurfHeader, HMM, [QuerySequence])
loadTestData files =
  do querySeqs <- readFasta $ fastaF files
     (header, model, _metadata) <- parse $ hmmPlusF files
     return (header, model, map (translateQuery . toStr . seqdata) querySeqs)
     
translateQuery :: String -> V.Vector Int
translateQuery = V.fromList . map lookup
  where lookup k = case V.elemIndex k Constants.amino of
                        Just i -> i
                        Nothing -> error "Residue not found in alphabet"

runCommand :: Commanded -> IO ()
runCommand (AlignmentSearch searchParams files) = run
  where hmmPlusFile = hmmPlusF files
        fastaFile = fastaF files
        run = do secPred <- getSecondary $ fastaFile
                 (header, hmm, queries) <- loadTestData files
                 let betas = getBetaStrands header

                 -- putStrLn $ show queries
                 let strat q = strategy searchParams hmm searchParams q betas
                 let scorer q = score hmm q betas

                 let searchQ r q = search (strat q) (scorer q) (newRandoms r)
                 let trySearch r q = if alignable q betas then searchQ r q else noSearch
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
                                          outputAlignment hmm betas ss query)
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


