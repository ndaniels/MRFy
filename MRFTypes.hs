module MRFTypes
where

import Data.Function
import Data.Ix
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List (sort, find, elemIndex, intercalate)
import qualified Data.Maybe as M
import qualified Data.Set as Set


import Score

type EmissionProbabilities = U.Vector Score
type InsertEmissions = EmissionProbabilities

type HMM = V.Vector HMMNode

data HMMNode = 
     HMMNode { nodeNum :: Int
             , matchEmissions :: EmissionProbabilities
             , insertionEmissions :: InsertEmissions
             , transitions :: StateTransitions
             }
             deriving (Show)

data HMMState = Mat | Ins | Del
                | Beg | End | BMat
                deriving (Show, Ord, Eq, Enum, Ix)

data TransitionProbabilities = 
     TransitionProbabilities { m_m :: TransitionProbability
                             , m_i :: TransitionProbability
                             , m_d :: TransitionProbability
                             , i_m :: TransitionProbability
                             , i_i :: TransitionProbability
                             , d_m :: TransitionProbability
                             , d_d :: TransitionProbability
                             , b_m :: TransitionProbability
                             , m_e :: TransitionProbability
                             }
                             deriving (Show)

-- mkTransProbs :: [Score] -> TransitionProbabilities
mkTransProbs t0 t1 t2 t3 t4 t5 t6 = TransitionProbabilities t0 t1 t2 t3 t4 t5 t6
                                                            (mkTransProb Beg Mat negLogZero)
                                                            (mkTransProb Mat End negLogZero)

data TransitionProbability = 
     TransitionProbability { logProbability :: Score
                           , fromState :: HMMState
                           , toState :: HMMState
                           } deriving (Show)

mkTransProb :: HMMState -> HMMState -> Score -> TransitionProbability
mkTransProb f t s = TransitionProbability s f t

type StateTransitions = TransitionProbabilities

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
