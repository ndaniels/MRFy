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
  )
where

import Debug.Trace (trace)

import Data.List (sort, find, elemIndex)
import qualified Data.Set as Set

import HmmPlus

type BetaPosition = Int

data BetaPair = BetaPair { pairPosition :: BetaPosition
                         , expose :: Exposure

                         -- info for quick indexing
                         , pairStrandSerial :: Int
                         , residueInd :: Int
                         } deriving (Show)
instance Eq BetaPair where
  p1 == p2 = (pairPosition p1) == (pairPosition p2)
instance Ord BetaPair where
  compare p1 p2 = compare (pairPosition p1) (pairPosition p2)

data BetaResidue = BetaResidue { resPosition :: BetaPosition
                               , resStrandSerial :: Int
                               , pairs :: [BetaPair]
                               }
instance Show BetaResidue where
  show r = "BetaResidue " ++ (show $ resPosition r) ++ (show $ pairs r)
instance Eq BetaResidue where
  r1 == r2 = (resPosition r1) == (resPosition r2)
instance Ord BetaResidue where
  compare r1 r2 = compare (resPosition r1) (resPosition r2)

data BetaStrand = BetaStrand { serial :: Int
                             , len :: Int
                             , residues :: [BetaResidue]
                             }
instance Show BetaStrand where
  show s = "BetaStrand " ++ (show $ serial s) ++ 
           " [" ++ (show $ residues s) ++ "]"
instance Eq BetaStrand where
  s1 == s2 = (serial s1) == (serial s2)

getBetaStrands :: SmurfHeader -> [BetaStrand]
getBetaStrands h = 
  addIndexInfo $ mkBetaStrands 1 $ (mkBetaResidues . getBetaPairs) h

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

mkBetaResidues :: [StrandPair] -> [BetaResidue]
mkBetaResidues sps = sort $ decResidues residues
  where residues = mkBetaResidues' sps

decResidues :: [BetaResidue] -> [BetaResidue]
decResidues residues = sort $ Set.elems $ Set.fromList $ decorated
  where decorated = decResidues' residues residues

decResidues' :: [BetaResidue] -> [BetaResidue] -> [BetaResidue]
decResidues' _ [] = []
decResidues' residues (r:rs) = nr : decResidues' residues rs
  where nr = foldl merge r $ filter ((==) r) residues

        merge :: BetaResidue -> BetaResidue -> BetaResidue
        merge b b' = b { pairs = Set.elems npairs }
          where npairs = (Set.fromList $ pairs b) 
                         `Set.union`
                         (Set.fromList $ pairs b')
        
mkBetaResidues' :: [StrandPair] -> [BetaResidue]
mkBetaResidues' [] = []
mkBetaResidues' (sp:sps) = newResidues ++ mkBetaResidues' sps
  where
        newResidues = case parallel sp of Parallel -> parallelPairs
                                          Antiparallel -> antiPairs
        parallelPairs = mkResidues $ zip3 (exposure sp) [s1..] [s2..]
        antiPairs = mkResidues $ zip3 (exposure sp) [s1..] [s2, s2-1..]
        s1 = firstStart sp
        s2 = secondStart sp + pairLength sp - 1

        mkResidues :: [(Exposure, BetaPosition, BetaPosition)] -> [BetaResidue]
        mkResidues [] = []
        mkResidues ((e, b1, b2):rest) = r1 : r2 : mkResidues rest
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

-- precondition: [BetaResidue] is sorted by 'position' in ascending order
mkBetaStrands :: Int -> [BetaResidue] -> [BetaStrand]
mkBetaStrands _ [] = []
mkBetaStrands i residues = mkBetaStrand : mkBetaStrands (i + 1) (reverse rest)
  where mkBetaStrand = BetaStrand { serial = i
                                  , len = length adjacent
                                  , residues = addSerial $ reverse adjacent 
                                  }
        addSerial = map (\r -> r { resStrandSerial = i })
        (adjacent, rest) = getAdjacentAndRest residues
        getAdjacentAndRest residues =
          foldl adjsAndRest ([], []) $ zip [startCount..] residues
        startCount = resPosition $ head residues
        adjsAndRest (adj, rest) (cnt, residue) =
          if cnt == resPosition residue then
            (residue:adj, rest)
          else
            (adj, residue:rest)

