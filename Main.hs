{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Array
import Data.List as DL
import System.Console.CmdArgs
import System.Random (getStdGen, randoms)
import qualified Data.Vector as V

import Bio.Sequence
import Bio.Sequence.Fasta

import Beta
-- import HmmAlign
import Viterbi
import HmmPlus
import Constants

import StochasticSearch
import qualified SearchStrategies.RandomHillClimb as RandomHillClimb
import qualified SearchStrategies.SimulatedAnnealing as SimulatedAnnealing

data SmurfArgs = SmurfArgs { hmmPlusFile :: FilePath
                           , fastaFile :: FilePath
                           }
  deriving (Show, Data, Typeable)

smurfargs = SmurfArgs { hmmPlusFile = def &= typ "HMM Plus file" &= argPos 0 
                      , fastaFile = def &= typ "FASTA file" &= argPos 1
                      }

-- to be removed
-- qseq = "STVWACIKLMAACDDEADGHSTVMMPQRRDDIKLMNPQSTVWYAGEADGE"
-- querySeq = "MVDDIFERGSKGSSDFFTGNVWVKMLVTDENGVFNTQVYDVVFEPGARTHWHSHPGGQILIVTRGKGFYQERGKPARILKKGDVVEIPPNVVHWHGAAPDEELVHIGISTQVHLGPAEWLGSVTEEEYRKATEGK" 

-- for an 8 bladed propeller
querySeq = "KDPANWVMTGRDYNAQNYSEMTDINKENVKQLRPAWSFSTGVLHGHEGTPLVVGDRMFIHTPFPNTTFALDLNEPGKILWQNKPKQNPTARTVACCDVVNRGLAYWPGDDQVKPLIFRTQLDGHIVAMDAETGETRWIMENSDIKVGSTLTIAPYVIKDLVLVGSSGAELGVRGYVTAYDVKSGEMRWRAFATGPDEELLLAEDFNAPNPHYGQKNLGLETWEGDAWKIGGGTNWGWYAYDPEVDLFYYGSGNPAPWNETMRPGDNKWTMAIWGREATTGEAKFAYQKTPHDEWDYAGVNVMMLSEQEDKQGQMRKLLTHPDRNGIVYTLDRTNGDLISADKMDDTVNWVKEVQLDTGLPVRDPEFGTRMDHKARDICPSAMGYHNQGHDSYDPERKVFMLGINHICMDWEPFMLPYRAGQFFVGATLTMYPGPKGDRGNASGLGQIKAYDAISGEMKWEKMERFSVWGGTMATAGGLTFYATLDGFIKARDSDTGDLLWKFKLPSGVIGHPMTYKHDGRQYVAIMYGVGGWPGVGLVFDLADPTAGLGSVGAFKRLQEFTQMGGGVMVFSLDGESPYSDPNVGEYAPGEPT"
-- querySeq = "KDPANWVMTGRDYNAQNYSEM" 

translateQuery :: String -> V.Vector Int
translateQuery = V.fromList . DL.map lookup
  where lookup k = case V.elemIndex k Constants.amino of
                        Just i -> i
                        Nothing -> error "Residue not found in alphabet"

-- qseq = "ADGE" 
-- querySeq = listArray (0, (length qseq) - 1) qseq

-- showAlignment :: HMM -> QuerySequence -> StatePath -> String 

outputAlignment :: HMM -> [BetaStrand] -> SearchSolution -> QuerySequence -> String
outputAlignment hmm betas ss querySeq = showAlignment hmm betas querySeq sp 60 Constants.amino
  where sp = statePath hmm querySeq betas ss

main = do sargs <- cmdArgs smurfargs
          (header, hmm, md) <- parse $ hmmPlusFile sargs
          rgn <- getStdGen
          querySeqs <- readFasta $ fastaFile sargs
          -- putStrLn $ show $ getBetaStrands header 
          -- putStrLn $ show $ viterbi (False, False) Constants.amino query hmm 
          -- putStrLn $ temp hmm 
          let betas = getBetaStrands header
          let queries = map (translateQuery . toStr . seqdata) querySeqs
          let results = map (\q -> search q hmm betas SimulatedAnnealing.ss ((randoms rgn) :: [Int])) queries
          -- putStrLn $ show $ (ss, hist) 
          putStrLn $ foldr (\s ss -> s ++ "\n\n" ++ ss) "" $ map (\((ss, hist), query) -> outputAlignment hmm betas ss query) $ zip results queries
          putStrLn $ "Score: " ++ (show $ fst $ fst $ head results)
          

