{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module MRFTypes
  ( MRF(..), HMM, HMMHeader(..), HMMNode(..), StateLabel(..)
  , matchEmissions, insertionEmissions  
  , StrandPair(..)
  , Helix(..)
  , Exposure(..), mkExposure
  , Direction(..), mkDirection
  , BetaStrand(..), BetaPosition, BetaResidue(..), BetaPair(..)
  , TProb(..), TProbs, m_m, m_i, m_d, i_m, i_i, d_m, d_d, b_m, m_e
  , mkTransProb, mkTransProbs
  , mkScore
  , showBetas
  )
where

import Data.Function
import Data.Ix
import Data.List -- (intercalate)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Score

type HMM = V.Vector HMMNode
type Vector a = U.Vector a

-- @ start hmmnode.tex
type EProbs = Vector Score
data HMMNode = HMMNode { nodeNum :: Int
                       , matEmissions :: EProbs
                       , insEmissions :: EProbs
                       , transitions  :: TProbs
-- @ end hmmnode.tex
-- @ start hmmnode.tex
                       }
-- @ end hmmnode.tex
             deriving (Show)

matchEmissions, insertionEmissions :: HMMNode -> EProbs
matchEmissions = matEmissions
insertionEmissions = insEmissions

-- @ start statelabel.tex
data StateLabel = Mat | Ins | Del | Beg | End
-- @ end statelabel.tex
                | BMat  -- keeping secrets from our readers...
                deriving (Show, Ord, Eq, Enum, Ix)

-- @ start tprob-tprobs.tex
newtype TProb = TProb { logProbability :: Score }
-- @ end tprob-tprobs.tex
           deriving (Show)

-- @ start tprob-tprobs.tex
data TProbs = TProbs
  { m_m :: TProb, m_i :: TProb, m_d :: TProb
  , i_m :: TProb, i_i :: TProb
-- @ end tprob-tprobs.tex
  ---GROSS HACKERY (these are in the middle to prevent a bad page break in the paper)
  , b_m :: TProb -- XXX aren't these just taking up space in the cache lines?
  , m_e :: TProb
-- @ start tprob-tprobs.tex
  , d_m :: TProb, d_d :: TProb }
-- @ end tprob-tprobs.tex
            deriving (Show)

-- | Don't ask. Probably something foul to do with HMMER
mkTransProbs :: TProb -> TProb -> TProb -> TProb -> TProb -> TProb -> TProb -> TProbs
mkTransProbs t0 t1 t2 t3 t4 t5 t6 =
  TProbs t0 t1 t2 t3 t4 nlz nlz t5 t6 -- gross, gross, gross
  where nlz = TProb negLogZero

mkTransProb :: StateLabel -> StateLabel -> Score -> TProb
mkTransProb _from _to s = TProb s

mkScore :: String -> Score
mkScore "*" = negLogZero
mkScore x   = Score $ read x

data StrandPair = StrandPair { firstStart  :: Int
                             , secondStart :: Int
                             , pairLength  :: Int
                             , parallel    :: Direction
                             , exposure    :: ExposureList
                             }
                             deriving (Show)

data Helix = Helix { startRes :: Int
                   , helixLength :: Int
                   }


data HMMHeader = HMMHeader { betas       :: [BetaStrand] -- TODO add alphas (sstructs)
                           , modelLength :: Int
                           , modelAlpha  :: String
                           , modelName   :: Maybe String
                           , modelVers   :: String
                           , modelMean   :: Maybe Double
                           , modelStdDev :: Maybe Double
                           }
                           deriving (Show)

data MRF = MRF { hmmHeader :: HMMHeader, hmm :: HMM }
               deriving (Show)

type ExposureList = [ Exposure ]

data Exposure = Buried -- 'i'
              | Exposed -- 'o'
              deriving (Show)

mkExposure :: Char -> Exposure
mkExposure c
         | c == 'i' = Buried
         | c == 'o' = Exposed
         | otherwise = error $ "Bad exposure " ++ [c]

data Direction = Parallel     -- "1"
               | Antiparallel -- "-1"
               deriving (Show)

mkDirection :: String -> Direction
mkDirection s
          | s == "1"  = Parallel
          | s == "-1" = Antiparallel
          | otherwise = error "Bogus direction string"

type BetaPosition = Int

data BetaStrand = BetaStrand { serial :: Int
                             , len :: Int
                             , residues :: [BetaResidue]
                             }

data BetaResidue = BetaResidue { resPosition :: BetaPosition
                               , resStrandSerial :: Int
                               , pairs :: [BetaPair]
                               }

data BetaPair = BetaPair { pairPosition :: BetaPosition
                         , expose :: Exposure

                         -- info for quick indexing
                         , pairStrandSerial :: Int
                         , residueInd :: Int
                         }

showBetas :: [BetaStrand] -> [Char]
showBetas betas = intercalate "\n" $ map show $ betas

instance Show BetaStrand where
  show s = "\nBetaStrand " ++ (show $ serial s) ++ 
           " [Length: " ++ (show $ len s) ++ "]" ++
           "\n" ++ (intercalate "\n" $ map show $ residues s)
instance Eq BetaStrand where
  s1 == s2 = (serial s1 == serial s2)
instance Ord BetaStrand where
  compare = compare `on` residues
  -- fixed previous version that gave phony equalities
  -- by considering only the first residue ---NR


instance Show BetaResidue where
  show r = "\tBetaResidue " ++ (show $ resPosition r) 
           ++ " [Strand serial: " ++ (show $ resStrandSerial r)
           ++ "\n" ++ (intercalate "\n" $ map show $ pairs r)
instance Eq BetaResidue where
  (==) = (==) `on` resPosition
instance Ord BetaResidue where
  compare = compare `on` resPosition


instance Show BetaPair where
  show p = "\t\tBetaPair " ++ (show $ pairPosition p) ++ 
           " [Exposure: " ++ (show $ expose p) ++ 
           ", Strand: " ++ (show $ pairStrandSerial p) ++
           ", Residue Index: " ++ (show $ residueInd p) ++ "]"

instance Eq BetaPair where
  (==) = (==) `on` pairPosition
instance Ord BetaPair where
  compare = compare `on` pairPosition
