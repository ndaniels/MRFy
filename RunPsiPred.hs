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
  do let dataDir = joinPath ["/", "home", "andrew", "data", "graduate", "research", "psipred", "data"]
     let pdata name = joinPath $ [dataDir, name]

     let binDir = "/home/andrew/data/graduate/research/psipred/bin/"
     let binary name = joinPath $ [binDir, name]

     cwdir <- getCurrentDirectory
     let fastaFile = joinPath $ [cwdir, fasta]

     mtx <- openFile (joinPath [tempDir, "mtx.mtx"]) WriteMode
     (_, _, _, seqph) <- createProcess (proc (binary "seq2mtx") [fastaFile]){ std_out = UseHandle mtx,
                                                             cwd = Just tempDir}

     waitForProcess seqph

     ss <- openFile (joinPath [tempDir, "ss"]) WriteMode
     (_, _, _, predh) <- createProcess (proc (binary "psipred") ["mtx.mtx", pdata "weights.dat", pdata "weights.dat2",
                                                    pdata "weights.dat3"]){ std_out = UseHandle ss,
                                                                            cwd = Just tempDir }

     waitForProcess predh

     trash <- openFile (joinPath ["/", "dev", "null"]) WriteMode
     (_, _, _, pred2h) <- createProcess (proc (binary "psipass2") [pdata "weights_p2.dat", "1", "1.0", "1.0",
                                                     "ss2", "ss"]){ cwd = Just tempDir, std_out = UseHandle trash }

     waitForProcess pred2h
     
     preds <- parsePsiPred $ joinPath [tempDir, "ss2"]

     -- preds <- ss_predictions $ psipred
     
     return (checkPreds preds)

getSecondary :: FilePath -> IO [SSPrediction]
getSecondary fasta = withTemporaryDirectory template (runPsiPred fasta)
-- getSecondary fasta = run_psipred fasta "/tmp/mrftemp" 
