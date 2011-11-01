{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Array
import System.Console.CmdArgs

import Beta
import HmmAlign
import HmmPlus

data SmurfArgs = SmurfArgs { hmmPlusFile :: FilePath }
  deriving (Show, Data, Typeable)

smurfargs = SmurfArgs { hmmPlusFile = def &= typ "HMM Plus file" &= argPos 0 }

-- to be removed
-- qseq = "STVWACIKLMAACDDEADGHSTVMMPQRRDDIKLMNPQSTVWYAGEADGE"
qseq = "MVDDIFERGSKGSSDFFTGNVWVKMLVTDENGVFNTQVYDVVFEPGARTHWHSHPGGQILIVTRGKGFYQERGKPARILKKGDVVEIPPNVVHWHGAAPDEELVHIGISTQVHLGPAEWLGSVTEEEYRKATEGK"
-- qseq = "ADGE" 
querySeq = listArray (0, (length qseq) - 1) qseq

main = do sargs <- cmdArgs smurfargs
          (header, hmm, md) <- parse $ hmmPlusFile sargs
          -- putStrLn $ show $ getBetaStrands header 
          putStrLn $ show $ viterbi_memo querySeq hmm
          

