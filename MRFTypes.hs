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
  , TScores(..)
  , EScores
  , mkTransScore, mkTransScores
  , mkScore
  , showBetas
  )
where

import Control.Applicative
import Data.Function
import Data.Ix
import Data.List -- (intercalate)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Prelude hiding (showList)
import Test.QuickCheck
import Text.Printf

import Score

type HMM = V.Vector HMMNode
type Vector a = U.Vector a

-- @ start hmmnode.tex
type EScores = Vector Score
data HMMNode = HMMNode { nodeNum :: Int
                       , matEmissions :: EScores
                       , insEmissions :: EScores
                       , transitions  :: TScores
-- @ end hmmnode.tex
-- @ start hmmnode.tex
                       }
-- @ end hmmnode.tex
instance Show HMMNode where
  show node =  "\n(#" ++ show (nodeNum node) ++ ": " ++ showTx (transitions node)
            ++ "; " ++ showEmit "M" (matEmissions node)
            ++ "; " ++ showEmit "I" (insEmissions node)
            ++ ")"

showList :: String -> [String] -> String
showList what [ ] = "no-" ++ what
showList _    ss  = intercalate ", " ss

showEmit :: String -> EScores -> String
showEmit pfx table = showList (pfx++"-emissions") $ map p $ filter nonzero $
                     zip (U.toList table) [0..]
  where p (score, aa) = pfx ++ show aa ++ "=" ++ sprintf "%.2f" score
        nonzero (s, _) = s < negLogZero
        p :: (Score, Int) -> String

matchEmissions, insertionEmissions :: HMMNode -> EScores
matchEmissions = matEmissions
insertionEmissions = insEmissions

-- @ start statelabel.tex
data StateLabel = Mat | Ins | Del | Beg | End
-- @ end statelabel.tex
                | BMat  -- keeping secrets from our readers...
                deriving (Show, Ord, Eq, Enum, Ix, Bounded)

-- @ start tprob-tprobs.tex
data TScores = TScores
  { m_m :: Score, m_i :: Score, m_d :: Score
  , i_m :: Score, i_i :: Score
  , d_m :: Score, d_d :: Score
  , b_m :: Score, m_e :: Score } -- legacy
-- @ end tprob-tprobs.tex
            deriving (Show, Eq)

data TxLabel = TL String String (TScores -> Score)
showTx :: TScores -> String
showTx t = showList "transitions" $ map tx $ filter nonzero
           [ TL "M" "M" m_m
           , TL "M" "I" m_i
           , TL "M" "D" m_d
           , TL "D" "M" d_m
           , TL "D" "D" d_d
           , TL "I" "M" i_m
           , TL "I" "I" i_i
           , TL "B" "M" b_m
           , TL "M" "E" m_e
           ]
  where tx (TL from to f) = from ++ "-" ++ tprintf "%.2f" (f t) ++ "->" ++ to
        tprintf fmt = sprintf fmt
        nonzero (TL _ _ f) = f t < negLogZero

-- | Don't ask. Probably something foul to do with HMMER
mkTransScores :: Score -> Score -> Score -> Score -> Score -> Score -> Score -> TScores
mkTransScores t0 t1 t2 t3 t4 t5 t6 =
  TScores { m_m = t0, m_i = t1, m_d = t2
          , i_m = t3, i_i = t4
          , d_m = t5, d_d = t6
          , b_m = nlz, m_e = nlz
          } 
  where nlz = negLogZero
        
sprintf :: PrintfType t => String -> Score -> t
sprintf fmt (Score s) = printf fmt s

mkTransScore :: StateLabel -> StateLabel -> Score -> Score
mkTransScore _from _to = id -- wtf?

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

instance Arbitrary StrandPair where
  arbitrary = StrandPair <$> first <*> second <*> len <*> d <*> e
    where (d, e) = (arbitrary, arbitrary)
          (first, second) = (choose (1, max), choose (1, max))
          len = choose (1, max)
          max = 100

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

type ExposureList = [Exposure]
-- newtype ExposureList = EList [ Exposure ] deriving Show 
--  
-- unElist :: ExposureList -> [Exposure] 
-- unElist (EList exposures) = exposures 
--  
-- instance Arbitrary ExposureList where 
  -- arbitrary = fmap EList $ listOf (arbitrary :: Gen Exposure) 
    

data Exposure = Buried -- 'i'
              | Exposed -- 'o'
              deriving (Show)

instance Arbitrary Exposure where
  arbitrary = elements [Buried, Exposed]

mkExposure :: Char -> Exposure
mkExposure c
         | c == 'i' = Buried
         | c == 'o' = Exposed
         | otherwise = error $ "Bad exposure " ++ [c]

data Direction = Parallel     -- "1"
               | Antiparallel -- "-1"
               deriving (Show)

instance Arbitrary Direction where
  arbitrary = elements [Parallel, Antiparallel]

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
