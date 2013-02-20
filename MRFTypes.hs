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
  , Query(..), loadQuery, translateQuery, QuerySequence
  )
where

import Control.Applicative
import Data.Function
import Data.Ix
import Data.List -- (intercalate)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck
import Text.Printf

import Bio.Sequence

import Constants
import Score

type QuerySequence = U.Vector AA

data Query = Query { qSeq    :: QuerySequence
                   , qHeader :: String
                   }
    deriving Show


loadQuery :: Sequence a -> Query
loadQuery sequence = Query s h
  where s = translateQuery $ toStr $ seqdata sequence
        h = toStr $ seqheader sequence

translateQuery :: String -> QuerySequence
translateQuery = U.fromList . map lookup
  where lookup k = case U.elemIndex k Constants.amino of
                        Just i -> AA i
                        Nothing -> error "Residue not found in alphabet"


type HMM = V.Vector HMMNode
type Vector a = U.Vector a

-- @ start hmmnode.tex
type EProbs = Vector Score
data HMMNode = HMMNode { nodeNum :: {-# UNPACK #-} !Int -- unpack all fields?
                       , matEmissions :: !EProbs
                       , insEmissions :: !EProbs
                       , transitions  :: !TProbs
-- @ end hmmnode.tex
-- @ start hmmnode.tex
                       }
-- @ end hmmnode.tex
instance Show HMMNode where
  show node =  "\n(#" ++ show (nodeNum node) ++ ": " ++ showTx (transitions node)
            ++ "; " ++ showEmit "M" (matEmissions node)
            ++ "; " ++ showEmit "I" (insEmissions node)
            ++ ")"

showEmit :: String -> EProbs -> String
showEmit pfx table = intercalate ", " $ zipWith p (U.toList table) [0..]
  where p score aa = pfx ++ show aa ++ "=" ++ sprintf "%.2f" score
        p :: Score -> Int -> String

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
  { m_m :: {-# UNPACK #-} !TProb, m_i :: {-# UNPACK #-} !TProb, m_d :: {-# UNPACK #-} !TProb
  , i_m :: {-# UNPACK #-} !TProb, i_i :: {-# UNPACK #-} !TProb
-- @ end tprob-tprobs.tex
  ---GROSS HACKERY (these are in the middle to prevent a bad page break in the paper)
  , b_m :: {-# UNPACK #-} !TProb -- XXX aren't these just taking up space in the cache lines?
  , m_e :: {-# UNPACK #-} !TProb
-- @ start tprob-tprobs.tex
  , d_m :: {-# UNPACK #-} !TProb, d_d :: {-# UNPACK #-} !TProb }
-- @ end tprob-tprobs.tex
            deriving (Show)

data TxLabel = TL String String (TProbs -> TProb)
showTx :: TProbs -> String
showTx t = intercalate ", " $ map tx [ TL "M" "M" m_m
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
        tprintf fmt = sprintf fmt . logProbability

-- | Don't ask. Probably something foul to do with HMMER
mkTransProbs :: TProb -> TProb -> TProb -> TProb -> TProb -> TProb -> TProb -> TProbs
mkTransProbs t0 t1 t2 t3 t4 t5 t6 =
  TProbs t0 t1 t2 t3 t4 nlz nlz t5 t6 -- gross, gross, gross
  where nlz = TProb negLogZero
        
sprintf :: PrintfType t => String -> Score -> t
sprintf fmt (Score s) = printf fmt s

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
