{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Array
import Data.List as DL
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

query :: Vector Int
query = fromList $ DL.map lookup querySeq
  where lookup k = case DL.elemIndex k Constants.aminoS of
                        Just i -> i
                        Nothing -> error "Residue not found in alphabet"

-- qseq = "ADGE" 
-- querySeq = listArray (0, (length qseq) - 1) qseq

-- showAlignment :: HMM -> QuerySequence -> StatePath -> String 

temp hmm = showAlignment hmm query sp 61 Constants.amino
  where (score, sp) = viterbi (False, False) Constants.amino query hmm

main = do sargs <- cmdArgs smurfargs
          (header, hmm, md) <- parse $ hmmPlusFile sargs
          -- putStrLn $ show $ getBetaStrands header 
          putStrLn $ show $ viterbi (False, False) Constants.amino query hmm
          putStrLn $ temp hmm
          

