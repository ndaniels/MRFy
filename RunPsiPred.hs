module RunPsiPred where

import Debug.Trace (trace)

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Unix.Directory

import PsiPred

template = "mrfy"
-- dataDir = joinPath ["/", "home", "andrew", "data", "graduate", "research", "psipred", "data"]
dataDir = joinPath ["/", "Users", "noah", "Downloads", "psipred32", "data"]

runPsiPred :: FilePath -> FilePath -> IO [SSPrediction]
runPsiPred fasta tempDir =
  do let pdata name = joinPath $ [dataDir, name]
     mtx <- trace (show tempDir) $ openFile (joinPath [tempDir, "mtx"]) WriteMode
     -- NMD note: when I fully specify the path to the process, this works.
     -- I think the proc function just isn't respecting path.
     -- easy solution: pull full path from env || hardcode
     (_, _, _, _) <- createProcess (proc "seq2mtx" [fasta]){ std_out = UseHandle mtx,
                                                             cwd = Just tempDir}

     ss <- openFile (joinPath [tempDir, "ss"]) WriteMode
     (_, _, _, _) <- createProcess (proc "psipred" ["mtx", pdata "weights.dat", pdata "weights.dat2",
                                                    pdata "weights.dat3"]){ std_out = UseHandle ss,
                                                                           cwd = Just tempDir }

     (_, _, _, _) <- createProcess (proc "psipass2" [pdata "weights_p2.dat", "1", "1.0", "1.0",
                                                     "ss2", "ss"]){ cwd = Just tempDir }
     (preds, md) <- parse $ joinPath [tempDir, "ss2"]
     return preds
          

getSecondary :: FilePath -> IO [SSPrediction]
getSecondary fasta = withTemporaryDirectory template (runPsiPred fasta)

