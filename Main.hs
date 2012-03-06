{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Parallel (par)
import Control.Parallel.Strategies

import Data.Array
import Data.List as DL
import System.Console.CmdArgs
import System.Random (getStdGen, mkStdGen, randoms)
import qualified Data.Vector as V
import System.Environment

import Bio.Sequence
import Bio.Sequence.Fasta

import Beta
-- import HmmAlign
import CommandArgs
import Constants
import HmmPlus
import RunPsiPred
import ShowAlignment
import StochasticSearch
import Viterbi


-- data SmurfArgs = SmurfArgs { hmmPlusFile :: FilePath 
                           -- , fastaFile :: FilePath 
                           -- } 
  -- deriving (Show, Data, Typeable) 

-- Maybe use System.Console.getopt instead
-- smurfargs = SmurfArgs { hmmPlusFile = def &= typ "HMM Plus file" &= argPos 0  
                      -- , fastaFile = def &= typ "FASTA file" &= argPos 1 
                      -- } 

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

popSearch :: [QuerySequence -> (SearchSolution, History)] -> QuerySequence -> (SearchSolution, History)
popSearch searches q = minimum $ (parMap rseq) (\s -> s q) searches

newRandoms s = randoms $ mkStdGen s

main = do argv <- getArgs
          (searchParams, files) <- getOpts argv
          let hmmPlusFile = hmmPlusF files
          let fastaFile = fastaF files
          (header, hmm, md) <- parse $ hmmPlusFile
          rgn <- getStdGen
          querySeqs <- readFasta $ fastaFile
          secPred <- getSecondary $ fastaFile
          -- let searchParams = searchP { secPreds = Just secPred } 
          -- let searchParams = searchP { secPreds = Just secPred } 
          -- putStrLn $ show $ getBetaStrands header 
          -- putStrLn $ show $ viterbi (False, False) Constants.amino query hmm 
          -- putStrLn $ temp hmm 
          let betas = getBetaStrands header
          let queries = map (translateQuery . toStr . seqdata) querySeqs
          let searches = map (\r -> (\q -> search q hmm betas searchParams (newRandoms r))) $ take (multiStartPopSize searchParams) ((randoms rgn) :: [Int])
          -- let results = map (\q -> search q hmm betas searchParams ((randoms rgn) :: [Int])) queries 
          let results = map (popSearch searches) queries
          -- putStrLn $ show $ (ss, hist) 
          putStrLn $ foldr (\s ss -> s ++ "\n\n" ++ ss) "" $ map (\((ss, hist), query) -> outputAlignment hmm betas ss query) $ zip results queries
          putStrLn $ "Score: " ++ (show $ fst $ fst $ head results)
          putStrLn $ "History: " ++ (show $ snd $ head results)
          

