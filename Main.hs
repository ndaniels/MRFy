{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Array
import System.Console.CmdArgs
import Data.Vector

import Beta
-- import HmmAlign
import Viterbi
import HmmPlus
import Constants

data SmurfArgs = SmurfArgs { hmmPlusFile :: FilePath }
  deriving (Show, Data, Typeable)

smurfargs = SmurfArgs { hmmPlusFile = def &= typ "HMM Plus file" &= argPos 0 }

-- to be removed
-- qseq = "STVWACIKLMAACDDEADGHSTVMMPQRRDDIKLMNPQSTVWYAGEADGE"
querySeq = "MVDDIFERGSKGSSDFFTGNVWVKMLVTDENGVFNTQVYDVVFEPGARTHWHSHPGGQILIVTRGKGFYQERGKPARILKKGDVVEIPPNVVHWHGAAPDEELVHIGISTQVHLGPAEWLGSVTEEEYRKATEGK"
-- qseq = "ADGE" 
-- querySeq = listArray (0, (length qseq) - 1) qseq

-- showAlignment :: HMM -> QuerySequence -> StatePath -> String 

temp hmm = showAlignment hmm querySeq sp 61 Constants.amino
  where (score, sp) = viterbi querySeq hmm Constants.amino

main = do sargs <- cmdArgs smurfargs
          (header, hmm, md) <- parse $ hmmPlusFile sargs
          -- putStrLn $ show $ getBetaStrands header 
          putStrLn $ show $ viterbi querySeq hmm Constants.amino
          putStrLn $ temp hmm
          

