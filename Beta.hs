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
import qualified Data.Maybe as M
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
instance Ord BetaStrand where
  compare s1 s2 = compare (firstRes s1) (firstRes s2)
    where firstRes = resPosition . head . residues


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
getBetaStrands h = addIndexInfo 
                   $ mkBetaStrands 
                   $ mergeStrands
                   $ addPairings 
                   $ (mkBetaResidues . getBetaPairs) h

-- Decorates the *pairs* with index information so that
-- Beta scoring is efficient later on.
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

-- Creates the BetaStrand structures and adds serial
-- information to each residue.
mkBetaStrands :: [[BetaResidue]] -> [BetaStrand]
mkBetaStrands = (mkBetaStrands' 0) . sort . map sort

mkBetaStrands' :: Int -> [[BetaResidue]] -> [BetaStrand]
mkBetaStrands' _ [] = []
mkBetaStrands' i (s:ss) = bs : mkBetaStrands' (i+1) ss
  where bs = BetaStrand { serial = i
                        , len = length s
                        , residues = map addSerial s
                        }

        addSerial n = n { resStrandSerial = i }

-- Merges overlapping strands.
-- In particular, forall S1 and S2 if S1 intersect S2 is not empty, then
-- merge S1 and S2 into one beta strand and remove S1 and S2.
mergeStrands :: [[BetaResidue]] -> [[BetaResidue]]
mergeStrands [] = []
mergeStrands (s:ss) = s' : (mergeStrands $ filter (emptyIntersection s') ss)
  where s' = maybeMerge s ss
  
        maybeMerge :: [BetaResidue] -> [[BetaResidue]] -> [BetaResidue]
        maybeMerge strand [] = strand
        maybeMerge strand (s:ss) = maybeMerge strand' ss
          where strand' = if emptyIntersection strand s then
                            strand
                          else
                            Set.elems $ (Set.fromList strand)
                                        `Set.union`
                                        (Set.fromList s)

        emptyIntersection :: Ord a => [a] -> [a] -> Bool
        emptyIntersection xs ys = Set.null $ (Set.fromList xs) 
                                             `Set.intersection` 
                                             (Set.fromList ys)

-- Using the word 'strand' here even though BetaStrands
-- haven't been made yet. Effectively, a strand is a list
-- of beta nodes.
addPairings :: [[BetaResidue]] -> [[BetaResidue]]
addPairings strands = map (map (\n -> foldl annotate n strands)) strands
  where -- Find one or zero equivalent nodes in a beta strand
        -- and add any additional pairs found to 'r'
        annotate :: BetaResidue -> [BetaResidue] -> BetaResidue
        annotate r = foldl merge r . filter ((==) r)
          where merge :: BetaResidue -> BetaResidue -> BetaResidue
                merge r r' = r { pairs = sort npairs }
                  where npairs = Set.elems $
                                  (Set.fromList $ pairs r)
                                  `Set.union`
                                  (Set.fromList $ pairs r')

mkBetaResidues :: [StrandPair] -> [[BetaResidue]]
mkBetaResidues [] = []
mkBetaResidues (sp:sps) = residues1 : residues2 : mkBetaResidues sps
  where (residues1, residues2) = mkResidues (zip3 (exposure sp) [s1..] secondIndexing) [] []

        secondIndexing = case parallel sp of Parallel -> [s2..]
                                             Antiparallel -> [s2, s2-1..]
        s1 = firstStart sp
        s2 = secondStart sp + pairLength sp - 1

        mkResidues :: [(Exposure, BetaPosition, BetaPosition)] -> 
                      [BetaResidue] -> [BetaResidue] ->
                      ([BetaResidue], [BetaResidue])
        mkResidues [] rs1 rs2 = (sort rs1, sort rs2)
        mkResidues ((e, b1, b2):rest) rs1 rs2 = mkResidues rest (r1:rs1) (r2:rs2)
          where r1 = BetaResidue { resPosition = b1
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

