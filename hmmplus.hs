{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables, NamedFieldPuns #-}

module Smurf.HmmPlus where
import Language.Pads.Padsc hiding (position, head)
import Language.Pads.GenPretty
import Control.Monad
import Data.List (sort)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

ws = REd "[\t ]+|$" " "

amino = "ACDEFGHIKLMNPQRSTVWY"
nucleotide = "ACTG"

[pads|
  data SmurfFile = SmurfFile { header::SmurfHeader, hmm::HMM <| (getAlphabet header, getNumNodes header) |> }
  
  type SmurfHeader = [Line HeaderLine] terminator Try (LitRE 'HMM ')
  
  data HeaderLine = HeaderLine { tag::Tag, ws, payload::Payload tag }
  
  data Tag = FileVersion "HMMER3/a" -- this string literal will change with major file version changes
            | NAME | ACC | DESC | LENG | ALPH | RF | CS | MAP | DATE | MEAN | RMSD
            | COM | NSEQ | EFFN | CKSUM | GA | TC | NC | STATS | BETA | Other (StringSE ws)
  
  data Payload (t::Tag) = case t of
      FileVersion -> Version VersionString
    | NAME -> Name StringLn
    | ACC -> Accession StringLn
    | DESC -> Description StringLn
    | LENG -> ModelLength Int
    | ALPH -> Alphabet StringLn
    | RF -> ReferenceAnnotation StringLn
    | CS -> ConsensusStructure StringLn
    | MAP -> MapAnnotation StringLn
    | DATE -> Date StringLn
    | COM -> CommandLog StringLn
    | NSEQ -> SequenceNumber Int
    | EFFN -> EffectiveSeq Double
    | CKSUM -> Checksum Int
    | GA -> PfamGathering (Double, ws, Double)
    | TC -> PfamTrusted (Double, ws, Double)
    | NC -> PfamNoise (Double, ws, Double)
    | STATS -> Stats {"LOCAL", ws, scoredist::ScoreDistribution, ws, values::[Double | ws] terminator Try EOR }
    | BETA -> Beta StrandPair
    | Other tag -> BadTag StringLn
    | otherwise -> OtherTag StringLn
    
  type VersionString = StringLn
  
  data ScoreDistribution = VLAMBDA | VMU | FTAU | MSV | VITERBI | FORWARD
  
   -- firstRes secondRes Length maxGap parallelism exposure
  data StrandPair = StrandPair {
        firstStart :: Int, ' ',
        secondStart :: Int, ' ',
        pairLength :: Int, ' ',
        maxGap :: Int, ' ',
        parallel :: Direction, ' ',
        exposure :: ExposureList
  }
    
  type ExposureList = [ Exposure ] terminator Try EOR
  data Exposure = In 'i'
                | Out 'o'  
                
  data Direction = Parallel "1"
                 | Antiparallel "-1"
  
  data HMM (alphabet::String, numNodes::Int) = HMM {
    "HMM", ws, hmmAlphabet::[Letter alphabet | ws] length <| length alphabet |>, ws, EOR,
    ws, transitionHeader::TransitionDescription, EOR,
    composition::Maybe (ws, "COMPO", ws, EmissionProbabilities alphabet, ws, EOR),
    insertZeroEmissions::InsertEmissions alphabet,
    stateZeroTransitions::StateTransitions,
    nodes::[HmmNode <| alphabet |>] terminator "//" where <| numNodes == length nodes |> }
--    rest::[StringLn | EOR] terminator EOF }
              
  type Letter (alphabet::String) = constrain c::Char where <| c `elem` alphabet |>
  
  type EmissionProbabilities (alphabet::String) = [ Double | ws ] length <| length alphabet |> 
  
--  type TransitionProbabilities (numStates::Int) = [ LogProbability | ws ] length numStates

  -- data TransitionProbabilities = TransitionProbabilities {
  --     m_m :: TransitionProbability <|(Match, Match)|>, ws,
  --     m_i :: TransitionProbability <|(Match, Insertion)|>, ws,
  --     m_d :: TransitionProbability <|(Match, Deletion)|>, ws,
  --     i_m :: TransitionProbability <|(Insertion, Match)|>, ws,
  --     i_i :: TransitionProbability <|(Insertion, Insertion)|>, ws,
  --     d_m :: TransitionProbability <|(Deletion, Match)|>, ws,
  --     d_d :: TransitionProbability <|(Deletion, Deletion)|>
  -- }
  
  data TransitionProbabilities = TransitionProbabilities {
      m_m :: TransitionProbability <|(0, 0)|>, ws,
      m_i :: TransitionProbability <|(0, 1)|>, ws,
      m_d :: TransitionProbability <|(0, 2)|>, ws,
      i_m :: TransitionProbability <|(1, 0)|>, ws,
      i_i :: TransitionProbability <|(1, 1)|>, ws,
      d_m :: TransitionProbability <|(2, 0)|>, ws,
      d_d :: TransitionProbability <|(2, 2)|>
  }
  
  type TransitionDescription = [ StringSE ws | ws] terminator (Try EOR)
  
  type InsertEmissions (alphabet::String) = (ws, EmissionProbabilities alphabet, ws, EOR)
  
  type StateTransitions = (ws, TransitionProbabilities, ws, EOR)
  
  data HmmNode (alphabet::String) = HmmNode {
                ws, nodeNum::Int, ws, matchEmissions::EmissionProbabilities alphabet, ws, annotations::EmissionAnnotationSet, EOR,
                insertionEmissions::InsertEmissions alphabet,
                transitions::StateTransitions
  }
  
  
  type EmissionAnnotationSet = (EmissionAnnotation, ws, EmissionAnnotation, ws, EmissionAnnotation)
  
  data EmissionAnnotation = MAPA Int
                          | Unused '-'
                          | RForCS Char
                          

  data LogProbability = LogZero '*'
                      | NonZero Double
                      
  -- data TransitionProbability (fState::HMMState, tState::HMMState) = TransitionProbability {  
                                                                  -- logProbability::LogProbability, 
                                                                  -- fromState = value fState::HMMState, 
                                                                  -- toState = value tState::HMMState 
  --  
  -- } 

  data TransitionProbability (fState::HMMState, tState::HMMState) = 
       TransitionProbability { logProbability::LogProbability
                             , fromState = value fState::HMMState
                             , toState = value tState::HMMState
                             }

    
  -- data HMMState = Match | Insertion | Deletion
  type HMMState = Int
                      
-- do we want these to be types or newtypes? newtype enforces type checking
-- but might prove cumbersome in the algorithm.
-- consider just making these type aliases.        
-- is there a way to make these newtypes but declare a mutual conversion so we can compare them?
-- we will want to take the max of several of these when we implement forward algorithm
  newtype FromMatch = FromMatch LogProbability
  type MatchToMatch = FromMatch
  type MatchToInsertion = FromMatch
  type MatchToDeletion = FromMatch
  newtype FromInsertion = FromInsertion LogProbability
  type InsertionToMatch = FromInsertion
  type InsertionToInsertion = FromInsertion
  newtype FromDeletion = FromDeletion LogProbability
  type DeletionToMatch = FromDeletion
  type DeletionToDeletion = FromDeletion

|]


-- instance Eq LogProbability where
--   x == y = case (x, y) of
--             (LogZero, LogZero) -> True
--             (NonZero x', NonZero y') -> x' == y'
--             (_, _) -> False

-- instance Ord LogProbability where
--   x < y = case (x, y) of
--             (LogZero, LogZero) -> False
--             (LogZero, _) -> True
--             (NonZero _, LogZero) -> False
--             (NonZero x', NonZero y') -> x' < y'


-- MatchToMatch < MatchToInsertion
-- lt :: MatchToMatch -> MatchToInsertion -> Bool
-- lt (MatchToMatch mp) (MatchToInsertion mp') = mp < mp'




-- I think we'll want a max function on a TransitionProbabilities, which gives us the max of the 7

getAlphabet :: SmurfHeader -> String
getAlphabet ((HeaderLine {tag, payload}):xs) = case tag of
                    ALPH -> case payload of
                              Alphabet "amino" -> amino
                              Alphabet "nucleotide" -> nucleotide
                              otherwise -> error "Invalid alphabet"
                    otherwise -> getAlphabet xs

getNumNodes :: SmurfHeader -> Int
-- getNumNodes header = 343 -- replace this  
getNumNodes ((HeaderLine {tag, payload}):xs) = case tag of
                    LENG -> case payload of
                              ModelLength i -> i
                              otherwise -> error "Invalid model length"
                    otherwise -> getNumNodes xs
                    
-- PARALLELISM:
-- start2 is the first residue in sequence in the second strand of the pair.
-- It is not necessarily the position aligned with the first strand's
-- first residue.
--
-- If parallel then start2 is paired with start1.
-- If antiparallel then start1 is paired with start2 + length - 1.
--
-- Exposures for strand 1 are simply read in order.
-- Exposures for strand 2:
--    If antiparallel: read in reverse.
--    If parallel:     read forwards.
--
-- An exposure applies to a *pair* of residues equally.
-- If s1 is paired with s2, then s2's exposure is the same as s1's exposure.
getBetaStrands :: SmurfHeader -> [BetaStrand]
getBetaStrands h = mkBetaStrands 1 $ (mkBetaResidues . getBetaPairs) h

getBetaPairs :: SmurfHeader -> [StrandPair]
getBetaPairs (HeaderLine {tag, payload}:xs) = 
  case tag of
       BETA -> case payload of 
                    Beta b -> b:getBetaPairs xs
                    otherwise -> error "Invalid beta"
       otherwise -> getBetaPairs xs                    
getBetaPairs [] = []                


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
        merge b b' = if solventExposure b /= solventExposure b' then
                       error $ "Bad exposure for residue " ++ show b ++ " and " ++ show b'
                     else
                       maybe b'' (setFwd b'') (pairFwd b'')
          where b'' = maybe b (setBck b) (pairBck b')

        setFwd :: BetaResidue -> BetaPosition -> BetaResidue
        setFwd b i = case pairFwd b of
                          Just i' -> if i /= i' then 
                                       error $ "Bad fwd pair for residue " ++ show b ++ ": (" ++ show i ++ ", " ++ show i' ++ ")"
                                     else
                                       b
                          Nothing -> b { pairFwd = Just i }

        setBck :: BetaResidue -> BetaPosition -> BetaResidue
        setBck b i = case pairBck b of
                          Just i' -> if i /= i' then 
                                       error $ "Bad bck pair for residue " ++ show b ++ ": (" ++ show i ++ ", " ++ show i' ++ ")"
                                     else
                                       b
                          Nothing -> b { pairBck = Just i }
  

mkBetaResidues' :: [StrandPair] -> [BetaResidue]
mkBetaResidues' [] = []
mkBetaResidues' (sp:sps) = newResidues ++ mkBetaResidues' sps
  where newResidues = case parallel sp of Parallel -> parallelPairs
                                          Antiparallel -> antiPairs
        parallelPairs = mkResidues $ zip3 (exposure sp) [s1..] [s2..]
        antiPairs = mkResidues $ 
                      zip3 (exposure sp) [s1..] [s2, s2-1..]
        s1 = firstStart sp
        s2 = secondStart sp + pairLength sp - 1

        mkResidues :: [(Exposure, BetaPosition, BetaPosition)] -> [BetaResidue]
        mkResidues [] = []
        mkResidues ((exp, b1, b2):rest) = p1 : p2 : mkResidues rest
          where p1 = BetaResidue { position = b1
                                 , solventExposure = exp
                                 , pairFwd = Just b2
                                 , pairBck = Nothing
                                 }
                p2 = BetaResidue { position = b2
                                 , solventExposure = exp
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


type BetaPosition = Int

data BetaResidue = BetaResidue { position :: BetaPosition
                               , solventExposure :: Exposure
                               , pairFwd :: Maybe BetaPosition
                               , pairBck :: Maybe BetaPosition
                               }
instance Show BetaResidue where
  show r = "BetaResidue " ++ (show $ position r) ++ details
    where details = " (Exposure: " ++ exp ++ ", Fwd: " ++ f ++ ", Bck: " ++ b ++ ")"
          exp = show $ solventExposure r
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


getTag :: Tag -> SmurfHeader -> Payload
getTag t ((HeaderLine {tag, payload}):xs) = if tag == t
                                              then payload
                                            else
                                              getTag t xs
getTag t [] = error "Tag not found"  
                  

result = do
        { (SmurfFile header hmm, md) <- parseFile "test.hmm+"
        ; return (header, hmm, md)
        }
