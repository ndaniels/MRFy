module RunPsiPred where

import Debug.Trace (trace)

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Unix.Directory

import PsiPred

template = "mrfy"

run_psipred :: FilePath -> FilePath -> IO [SSPredictions]
run_psipred fasta tempDir =
  do let dataDir = joinPath ["/", "home", "andrew", "data", "graduate", "research", "psipred", "data"]
     mtx <- trace (show tempDir) $ openFile (joinPath [tempDir, "mtx"]) WriteMode
     (_, _, _, _) <- createProcess (proc "seq2mtx" [fasta]){ std_out = UseHandle mtx,
                                                             cwd = tempDir}

     ss <- openFile (joinPath [tempDir, "ss"]) WriteMode
     (_, _, _, _) <- createProcess (proc "psipred" ["mtx", pdata "weights.dat", pdata "weights.dat2",
                                                    pdata "weights.dat3"]{ std_out = UseHandle ss,
                                                                           cwd = tempDir }

     (_, _, _, _) <- createProcess (proc "psipass2" [pdata "weights_p2.dat", "1", "1.0", "1.0",
                                                     "ss2" "ss"]{ cwd = tempDir }

     return $ getSecondary $ joinPath [tempDir, "ss2"]
  where pdata name = joinPath $ [dataDir, name]

getSecondary :: FilePath -> IO [SSPredictions]
getSecondary fasta = withTemporaryDirectory template (run_psipred fasta)

