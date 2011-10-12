{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables, NamedFieldPuns #-}
module HmmPlus 
  ( SmurfHeader
  
  , Exposure
  , StrandPair
  , firstStart
  , secondStart
  , pairLength
  , maxGap
  , parallel
  , exposure
  
  , paraOrAnti
  , getBetaPairs
  , parse
  )
where

import Language.Pads.Padsc hiding (position, head)
import Language.Pads.GenPretty
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

ws = REd "[\t ]+|$" " "

amino = "ACDEFGHIKLMNPQRSTVWY"
nucleotide = "ACTG"

[pads|
  data SmurfFile = SmurfFile { header::SmurfHeader, 
                               hmm::HMM <| (getAlphabet header, 
                                            getNumNodes header) |> }
  
  type SmurfHeader = [Line HeaderLine] terminator Try (LitRE 'HMM ')
  
  data HeaderLine = HeaderLine { tag::Tag, ws, payload::Payload tag }
  
  data Tag = FileVersion "HMMER3/a" -- this string literal will change with 
                                    -- major file version changes
            | NAME | ACC | DESC | LENG | ALPH | RF | CS | MAP | DATE | MEAN | RMSD
            | COM | NSEQ | EFFN | CKSUM | GA | TC | NC | STATS | BETA | Other (StringSE ws)
  
  data Payload (t::Tag) = case t of
      FileVersion -> Version VersionString
    | NAME -> Name StringLn
    | ACC -> Accession StringLn
    | DESC -> Description StringLn
    | LENG -> ModelLength Int
    | ALPH -> Alphabet StringLn -- amino or nucleotide
    | RF -> ReferenceAnnotation StringLn
    | CS -> ConsensusStructure StringLn
    | MAP -> MapAnnotation StringLn
    | DATE -> Date StringLn
    | MEAN -> Mean Double -- used to calculate p-value
    | RMSD -> StandardDeviation Double -- used to calculate p-value
    | COM -> CommandLog StringLn
    | NSEQ -> SequenceNumber Int
    | EFFN -> EffectiveSeq Double
    | CKSUM -> Checksum Int
    | GA -> PfamGathering (Double, ws, Double)
    | TC -> PfamTrusted (Double, ws, Double)
    | NC -> PfamNoise (Double, ws, Double)
    | STATS -> Stats {"LOCAL", ws, scoredist::ScoreDistribution, ws, values::[Double | ws] terminator Try EOR }
    | BETA -> Beta StrandPair -- consensus beta-strand pairing
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
  
  data HMM (alphabet :: String, numNodes :: Int) = 
       HMM { "HMM"
           , ws
           -- the hmmAlphabet is just informational, but it
           -- helps identify (positionally) which EmissionProbabilities maps
           -- to which residue letter
           , hmmAlphabet :: [Letter alphabet | ws] length <| length alphabet |>
           , ws
           , EOR
           , ws
           -- the transitionHeader is just informational, but it
           -- helps identify (positionally) which TransitionProbabilities map
           -- from which source state to which destination state
           -- such as 'm->m' and 'm->i'
           , transitionHeader :: TransitionDescription
           , EOR
           -- the model's overall match state probabilities; optional
           -- I do not believe the Smurf2 algorithm has any use for these.
           -- They are used for null model filtering in HMMER, but we cannot
           -- use that simple 1-state null model in Smurf2.
           , composition :: Maybe (ws, "COMPO", ws, 
                                   EmissionProbabilities alphabet, ws, EOR)
           -- the insertZeroEmissions and stateZeroTransitions are the
           -- transition probabilities for the BEGIN node of the whole HMM.
           -- match state 0 is the BEGIN state, which is mute so has no
           -- match emission probabilities. the insertZeroEmissions is
           -- the emission probability table for an insert state that may occur
           -- before the first real match state (M1)                                    
           , insertZeroEmissions :: InsertEmissions alphabet
           -- the stateZeroTransitions are the transition probabilities for
           -- b->m1, b->i0, b->d1, i0->m1, i0->i0, d0->m1 (always 0.0), d0->d1 (always *)
           -- recall that these are log probabilities, so log1 == 0 and log0 == infinity (*)
           , stateZeroTransitions :: StateTransitions
           -- the "regular" nodes, which are the rest of the state transition and emission
           -- probabilities. See the HmmNode type for full documentation
           , nodes :: [HmmNode <| alphabet |>] terminator "//" 
             where <| numNodes == length nodes |>
           }
              
  type Letter (alphabet :: String) = constrain c :: Char 
                                     where <| c `elem` alphabet |>
  
  type EmissionProbabilities (alphabet :: String) = 
       [ Double | ws ] length <| length alphabet |> 

  -- A bit of a hack here, because Pads doesn't yet support non-base types
  -- as the parameteter for an algebraic parser type
  -- note that 0 is for a match, 1 for insertion, and 2 for a deletion
  -- 
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
  
  type InsertEmissions (alphabet :: String) = 
       (ws, EmissionProbabilities alphabet, ws, EOR)
  
  type StateTransitions = (ws, TransitionProbabilities, ws, EOR)
  
  data HmmNode (alphabet::String) = 
       HmmNode { ws
               -- simply the index (number) of the node. Begin state is 0.
               , nodeNum :: Int
               , ws
               -- Emission log-odds probabilities for the match state
               -- remember these are mapped to the alphabet in alphabetic order
               , matchEmissions :: EmissionProbabilities alphabet
               , ws
               -- these are three extra fields for MAP, RF, and CS
               -- we do not use them in the Smurf2 algorithm; see the
               -- HMMER3 user guide if you are curious.
               , annotations :: EmissionAnnotationSet
               , EOR
               -- these fields are the insert emission log-odds scores, one per
               -- symbol in alphabetic order
               , insertionEmissions :: InsertEmissions alphabet
               -- these fields are the transition log-odds for this node in order:
               -- mk->(mk+1, ik, dk+1), ik->(mk+1, ik); dk->(mk+1, dk+1)
               -- note that these correspond exactly to the edges in a profile
               -- HMM state-transition diagram
               , transitions::StateTransitions
               }
  
  type EmissionAnnotationSet = ( EmissionAnnotation
                               , ws
                               , EmissionAnnotation
                               , ws
                               , EmissionAnnotation
                               )
  
  data EmissionAnnotation = MAPA Int
                          | Unused '-'
                          | RForCS Char
                          

  data LogProbability = LogZero '*'
                      | NonZero Double
                      

  data TransitionProbability (fState::HMMState, tState::HMMState) = 
       TransitionProbability { logProbability::LogProbability
                             , fromState = value fState::HMMState
                             , toState = value tState::HMMState
                             }

  type HMMState = Int
                      
  -- do we want these to be types or newtypes? newtype enforces type checking
  -- but might prove cumbersome in the algorithm.
  -- consider just making these type aliases.        
  -- is there a way to make these newtypes but declare a mutual conversion 
  -- so we can compare them?
  -- we will want to take the max of several of these when we implement 
  -- forward algorithm
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
getNumNodes ((HeaderLine {tag, payload}):xs) = case tag of
                    LENG -> case payload of
                              ModelLength i -> i
                              otherwise -> error "Invalid model length"
                    otherwise -> getNumNodes xs
                    


getTag :: Tag -> SmurfHeader -> Payload
getTag t ((HeaderLine {tag, payload}):xs) = if tag == t
                                              then payload
                                            else
                                              getTag t xs
getTag t [] = error "Tag not found"  

-- This is a hack since I cannot seem to expose the type constructors for
-- Direction. (Which is declared in the Pads flib flab above.)
paraOrAnti :: Direction -> a -> a -> a
paraOrAnti p when_para when_anti = case p of Parallel -> when_para
                                             Antiparallel -> when_anti

getBetaPairs :: SmurfHeader -> [StrandPair]
getBetaPairs (HeaderLine {tag = tag, payload = payload}:xs) = 
  case tag of
       BETA -> case payload of 
                    Beta b -> b:getBetaPairs xs
                    otherwise -> error "Invalid beta"
       otherwise -> getBetaPairs xs                    
getBetaPairs [] = []                
                
-- What is "SmurfFile_md" ? o_0
-- It is the metadata from the PADS parser, and can give us information
-- as to errors in the file format! We'll want to do some error checking
-- and fail gracefully.
parse :: FilePath -> IO (SmurfHeader, HMM, SmurfFile_md)
parse f = do (SmurfFile header hmm, md) <- parseFile f
             return (header, hmm, md)

