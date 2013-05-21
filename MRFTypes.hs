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
  , TProb(..), TProbs(..)
  , EProbs
  , mkTransProb, mkTransProbs
  , mkScore
  , showBetas
  , hmmDot, hmmDotString
  )
where

import Control.Applicative
import Data.Char()
import Data.Function
import Data.Ix
import Data.List -- (intercalate)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Prelude hiding (showList)
import Test.QuickCheck
import Text.Printf

import qualified Dot
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
instance Show HMMNode where
  show node =  "\n(#" ++ show (nodeNum node) ++ ": " ++ showTx (transitions node)
            ++ "; " ++ showEmit "M" (matEmissions node)
            ++ "; " ++ showEmit "I" (insEmissions node)
            ++ ")"

showList :: String -> [String] -> String
showList what [ ] = "no-" ++ what
showList _    ss  = intercalate ", " ss

showEmit :: String -> EProbs -> String
showEmit pfx table = showList (pfx++"-emissions") $ map p $ filter nonzero $
                     zip (U.toList table) [0..]
  where p (score, aa) = pfx ++ show aa ++ "=" ++ sprintf "%.2f" score
        nonzero (s, _) = s < negLogZero
        p :: (Score, Int) -> String

hmmDotString :: HMM -> String
hmmDotString = Dot.toDot . hmmDot

hmmDot :: HMM -> Dot.DotBuilder ()
hmmDot nodes =
  do e <- Dot.node "E"
     ed <- Dot.node "ED"
     node (V.length nodes - 1) e ed
 where node 0 m d = do b <- Dot.node ("B" ++ emissions (matEmissions n))
                       i <- Dot.node ("I" ++ emissions (insEmissions n))
                       edge b i (m_i trans)
                       edge b m (m_m trans)
                       edge b d (m_d trans)
                       edge i i (i_i trans)
                       edge i m (i_m trans)
         where n = nodes V.! 0
               trans = transitions n
                       
       node j m d = do m' <- Dot.node ("M" ++ emissions (matEmissions n))
                       i  <- Dot.node ("I" ++ emissions (insEmissions n))
                       d' <- Dot.node "D"
                       edge m' i (m_i trans)
                       edge m' m (m_m trans)
                       edge m' d (m_d trans)
                       edge i i (i_i trans)
                       edge i m (i_m trans)
                       edge d' m (d_m trans)
                       edge d' d (d_d trans)
                       node (pred j) m' d'
         where n = nodes V.! j
               trans = transitions n
                       
       edge from to (TProb p) =
         if p < negLogZero then Dot.edge from to (sprintf "%.2f" p)
         else return ()

       emissions :: EProbs -> String
       emissions v = concat $ map p $ filter nonzero $ zip [0..] (U.toList v)
         where p (i, (Score score)) = printf "\\nAA%d = %.2f" i score
               p :: (Int, Score) -> String
               nonzero (_, s) = s < negLogZero
                       
                       

matchEmissions, insertionEmissions :: HMMNode -> EProbs
matchEmissions = matEmissions
insertionEmissions = insEmissions

-- @ start statelabel.tex
data StateLabel = Mat | Ins | Del | Beg | End
-- @ end statelabel.tex
                | BMat  -- keeping secrets from our readers...
                deriving (Show, Ord, Eq, Enum, Ix, Bounded)

-- @ start tprob-tprobs.tex
newtype TProb = TProb { logProbability :: Score }
-- @ end tprob-tprobs.tex
           deriving (Show, Eq)

-- @ start tprob-tprobs.tex
data TProbs = TProbs
  { m_m :: TProb, m_i :: TProb, m_d :: TProb
  , i_m :: TProb, i_i :: TProb
  , d_m :: TProb, d_d :: TProb
  , b_m :: TProb, m_e :: TProb } -- legacy
-- @ end tprob-tprobs.tex
            deriving (Show, Eq)

data TxLabel = TL String String (TProbs -> TProb)
showTx :: TProbs -> String
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
        tprintf fmt = sprintf fmt . logProbability
        nonzero (TL _ _ f) = logProbability (f t) < negLogZero

-- | Don't ask. Probably something foul to do with HMMER
mkTransProbs :: TProb -> TProb -> TProb -> TProb -> TProb -> TProb -> TProb -> TProbs
mkTransProbs t0 t1 t2 t3 t4 t5 t6 =
  TProbs { m_m = t0, m_i = t1, m_d = t2
         , i_m = t3, i_i = t4
         , d_m = t5, d_d = t6
         , b_m = nlz, m_e = nlz
         } 
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
