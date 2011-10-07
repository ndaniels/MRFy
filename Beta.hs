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
  ( getBetaStrands )
where

import Data.List (sort)
import qualified Data.Set as Set

import HmmPlus

type BetaPosition = Int

data BetaResidue = BetaResidue { position :: BetaPosition
                               , exposureFwd :: Maybe Exposure
                               , exposureBck :: Maybe Exposure
                               , pairFwd :: Maybe BetaPosition
                               , pairBck :: Maybe BetaPosition
                               }
instance Show BetaResidue where
  show r = "BetaResidue " ++ (show $ position r) ++ details
    where details = " (Exposure Fwd: " ++ expF ++ ", Exposure Bck: " ++ expB 
                      ++ ", Pair Fwd: " ++ f ++ ", Pair Bck: " ++ b ++ ")"
          expF = show $ exposureFwd r
          expB = show $ exposureBck r
          f = show $ pairFwd r
          b = show $ pairBck r
instance Eq BetaResidue where
  r1 == r2 = (position r1) == (position r2)
instance Ord BetaResidue where
  compare r1 r2 = compare (position r1) (position r2)

data BetaStrand = BetaStrand { serial :: Int
                             , residues :: [BetaResidue]
                             }
instance Show BetaStrand where
  show s = "BetaStrand " ++ (show $ serial s) ++ 
           " [" ++ (show $ residues s) ++ "]"
instance Eq BetaStrand where
  s1 == s2 = (serial s1) == (serial s2)

getBetaStrands :: SmurfHeader -> [BetaStrand]
getBetaStrands h = mkBetaStrands 1 $ (mkBetaResidues . getBetaPairs) h

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
        merge b b' = maybe b'' (setFwd b'') (pairFwd b'')
          where b'' = maybe b (setBck b) (pairBck b')

        setFwd :: BetaResidue -> BetaPosition -> BetaResidue
        setFwd b i = case pairFwd b of
                          Just i' -> if i /= i' then 
                                       error $ "Bad fwd pair for residue " 
                                                ++ show b ++ ": (" ++ show i 
                                                ++ ", " ++ show i' ++ ")"
                                     else
                                       b
                          Nothing -> b { pairFwd = Just i }

        setBck :: BetaResidue -> BetaPosition -> BetaResidue
        setBck b i = case pairBck b of
                          Just i' -> if i /= i' then 
                                       error $ "Bad bck pair for residue " 
                                                ++ show b ++ ": (" ++ show i 
                                                ++ ", " ++ show i' ++ ")"
                                     else
                                       b
                          Nothing -> b { pairBck = Just i }
  

mkBetaResidues' :: [StrandPair] -> [BetaResidue]
mkBetaResidues' [] = []
mkBetaResidues' (sp:sps) = newResidues ++ mkBetaResidues' sps
  where
        newResidues = paraOrAnti (parallel sp) parallelPairs antiPairs
        parallelPairs = mkResidues $ zip3 (exposure sp) [s1..] [s2..]
        antiPairs = mkResidues $ 
                      zip3 (exposure sp) [s1..] [s2, s2-1..]
        s1 = firstStart sp
        s2 = secondStart sp + pairLength sp - 1

        mkResidues :: [(Exposure, BetaPosition, BetaPosition)] -> [BetaResidue]
        mkResidues [] = []
        mkResidues ((exp, b1, b2):rest) = p1 : p2 : mkResidues rest
          where p1 = BetaResidue { position = b1
                                 , exposureFwd = Just exp
                                 , exposureBck = Nothing
                                 , pairFwd = Just b2
                                 , pairBck = Nothing
                                 }
                p2 = BetaResidue { position = b2
                                 , exposureFwd = Nothing
                                 , exposureBck = Just exp
                                 , pairFwd = Nothing
                                 , pairBck = Just b1
                                 }

-- precondition: [BetaResidue] is sorted by 'position' in ascending order
mkBetaStrands :: Int -> [BetaResidue] -> [BetaStrand]
mkBetaStrands _ [] = []
mkBetaStrands i residues = mkBetaStrand : mkBetaStrands (i + 1) (reverse rest)
  where mkBetaStrand = BetaStrand { serial = i, residues = reverse adjacent }
        (adjacent, rest) = getAdjacentAndRest residues
        getAdjacentAndRest residues =
          foldl adjsAndRest ([], []) $ zip [startCount..] residues
        startCount = position $ head residues
        adjsAndRest (adj, rest) (cnt, residue) =
          if cnt == position residue then
            (residue:adj, rest)
          else
            (adj, residue:rest)

