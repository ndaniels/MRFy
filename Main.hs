{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Array
import Data.List as DL
import System.Console.CmdArgs
import System.Random (getStdGen, randoms)
import qualified Data.Vector as V

import Beta
-- import HmmAlign
import Viterbi
import HmmPlus
import Constants

import StochasticSearch
import qualified SearchStrategies.RandomHillClimb as RandomHillClimb

data SmurfArgs = SmurfArgs { hmmPlusFile :: FilePath }
  deriving (Show, Data, Typeable)

smurfargs = SmurfArgs { hmmPlusFile = def &= typ "HMM Plus file" &= argPos 0 }

-- to be removed
-- qseq = "STVWACIKLMAACDDEADGHSTVMMPQRRDDIKLMNPQSTVWYAGEADGE"
querySeq = "MVDDIFERGSKGSSDFFTGNVWVKMLVTDENGVFNTQVYDVVFEPGARTHWHSHPGGQILIVTRGKGFYQERGKPARILKKGDVVEIPPNVVHWHGAAPDEELVHIGISTQVHLGPAEWLGSVTEEEYRKATEGK"

query :: V.Vector Int
query = V.fromList $ DL.map lookup querySeq
  where lookup k = case V.elemIndex k Constants.amino of
                        Just i -> i
                        Nothing -> error "Residue not found in alphabet"

-- qseq = "ADGE" 
-- querySeq = listArray (0, (length qseq) - 1) qseq

-- showAlignment :: HMM -> QuerySequence -> StatePath -> String 

-- temp hmm = showAlignment hmm query sp 61 Constants.amino
--   where (score, sp) = viterbi (False, False) Constants.amino query hmm

main = do sargs <- cmdArgs smurfargs
          (header, hmm, md) <- parse $ hmmPlusFile sargs
          rgn <- getStdGen
          -- putStrLn $ show $ getBetaStrands header 
          -- putStrLn $ show $ viterbi (False, False) Constants.amino query hmm 
          -- putStrLn $ temp hmm 
          putStrLn $ show $ search query hmm (getBetaStrands header) RandomHillClimb.ss ((randoms rgn) :: [Int])
          

