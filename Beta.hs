{--
start2 is the first residue in sequence in the second strand of the pair.
It is not necessarily the position aligned with the first strand's
first residue.

If parallel then start2 is paired with start1.
If antiparallel then start1 is paired with start2 + length - 1.

Exposures for strand 1 are simply read in order.
Exposures for strand 2:
   If antiparallel: read in reverse.
   If parallel:     read forwards.

An exposure applies to a *pair* of residues equally.
If s1 is paired with s2, then s2's exposure is the same as s1's exposure.
--}

module Beta 
  ( getBetaStrands
  , BetaStrand (..)
  , BetaResidue (..)
  , BetaPair (..)
  , showBetas
  )
where

import Debug.Trace (trace)

import Data.List (sort, find, elemIndex, intercalate)
import qualified Data.Set as Set

import HmmPlus

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
  s1 == s2 = (serial s1) == (serial s2)


instance Show BetaResidue where
  show r = "\tBetaResidue " ++ (show $ resPosition r) 
           ++ " [Strand serial: " ++ (show $ resStrandSerial r)
           ++ "\n" ++ (intercalate "\n" $ map show $ pairs r)
instance Eq BetaResidue where
  r1 == r2 = (resPosition r1) == (resPosition r2)
instance Ord BetaResidue where
  compare r1 r2 = compare (resPosition r1) (resPosition r2)


instance Show BetaPair where
  show p = "\t\tBetaPair " ++ (show $ pairPosition p) ++ 
           " [Exposure: " ++ (show $ expose p) ++ 
           ", Strand: " ++ (show $ pairStrandSerial p) ++
           ", Residue Index: " ++ (show $ residueInd p) ++ "]"

instance Eq BetaPair where
  p1 == p2 = (pairPosition p1) == (pairPosition p2)
instance Ord BetaPair where
  compare p1 p2 = compare (pairPosition p1) (pairPosition p2)

getBetaStrands :: SmurfHeader -> [BetaStrand]
getBetaStrands h = 
  addIndexInfo $ (mkBetaResidues . getBetaPairs) h

addIndexInfo :: [BetaStrand] -> [BetaStrand]
addIndexInfo betas = addIndexInfo' betas
  where addIndexInfo' [] = []
        addIndexInfo' (b:bs) = b' : addIndexInfo' bs
          where b' = b { residues = map addToResidue $ residues b }

        addToResidue :: BetaResidue -> BetaResidue
        addToResidue r = r { pairs = map (addToPair betas) $ pairs r }
          where addToPair [] _ = error "could not find beta position in list of beta strands"
                addToPair (b:bs) p = 
                  case find (\r' -> pairPosition p == resPosition r') $ residues b of
                    Just r' -> p { pairStrandSerial = resStrandSerial r'
                                 , residueInd = maybe (error "whoa there") id $ elemIndex r' $ residues b
                                 }
                    Nothing -> addToPair bs p

mkBetaResidues :: [StrandPair] -> [BetaStrand]
mkBetaResidues sps = mkBetaResidues' sps [] 0

mkBetaResidues' :: [StrandPair] -> [BetaStrand] -> Int -> [BetaStrand]
mkBetaResidues' [] strands i = strands
mkBetaResidues' (sp:sps) strands i = mkBetaResidues' sps (strands' ++ nstrand1 ++ nstrand2) i''
  where nstrand1 = if null residues1 then [] else [mkStrand i residues1]
        nstrand2 = if null residues2 then [] else [mkStrand i' residues2]
        (residues1, residues2, strands') =  -- residues1 XOR residues2 could be empty!
          mkResidues (zip3 (exposure sp) [s1..] secondIndexing) [] [] strands

        -- Don't increment 'i' for a strand we didn't make...
        i'' = if null residues2 then i' else i' + 1
        i' = if null residues1 then i else i + 1

        secondIndexing = case parallel sp of Parallel -> [s2..]
                                             Antiparallel -> [s2, s2-1..]
        s1 = firstStart sp
        s2 = secondStart sp + pairLength sp - 1

        mkStrand :: Int -> [BetaResidue] -> BetaStrand
        mkStrand ind rs = BetaStrand { serial = ind
                                     , len = length rs
                                     , residues = map (\r -> r { resStrandSerial = ind }) rs
                                     }

        -- If ANY strand has ANY residue with position 'b'
        strandExists :: BetaPosition -> Bool
        strandExists b = any (\s -> any (\r -> b == resPosition r) (residues s)) strands

        annotate :: [BetaStrand] -> BetaResidue -> [BetaStrand]
        annotate [] _ = []
        annotate (s:ss) r = s' : annotate ss r
          where s' = s { residues = map addPairings $ residues s }
                addPairings r' = if r' /= r then r'
                                 else r' { pairs = Set.elems $ (Set.fromList $ pairs r) `Set.union` (Set.fromList $ pairs r') }

        mkResidues :: [(Exposure, BetaPosition, BetaPosition)] -> [BetaResidue] -> [BetaResidue] -> [BetaStrand] -> ([BetaResidue], [BetaResidue], [BetaStrand])
        mkResidues [] rs1 rs2 ss = (rs1, rs2, ss)
        mkResidues ((e, b1, b2):rest) rs1 rs2 ss = mkResidues rest rs1' rs2' ss''
          where rs1' = if b1Exists then rs1 else (r1:rs1)
                rs2' = if b2Exists then rs2 else (r2:rs2)

                ss'' = if b2Exists then annotate ss' r2 else ss'
                ss' = if b1Exists then annotate ss r1 else ss

                b1Exists = strandExists b1
                b2Exists = strandExists b2

                r1 = BetaResidue { resPosition = b1
                                 , resStrandSerial = undefined
                                 , pairs = [p1]
                                 }
                p1 = BetaPair { pairPosition = b2
                              , expose = e
                              , pairStrandSerial = undefined
                              , residueInd = undefined
                              }
                r2 = BetaResidue { resPosition = b2
                                 , resStrandSerial = undefined
                                 , pairs = [p2]
                                 }
                p2 = BetaPair { pairPosition = b1
                              , expose = e
                              , pairStrandSerial = undefined
                              , residueInd = undefined
                              }

