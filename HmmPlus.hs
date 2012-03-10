{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables, NamedFieldPuns #-}
module HmmPlus 
  ( SmurfHeader
  
  , HMM(..)
  , HMMState(..)
  , HmmNode(..)
  , Direction(..)
  , StateAcc
  , TransitionProbability(..)
  , TransitionProbabilities(..)
  , LogProbability(..)
  , Exposure(..)
  , StrandPair
  , firstStart
  , secondStart
  , pairLength
  , maxGap
  , parallel
  , exposure
  
  , getBetaPairs
  , parse
  )
where

import Debug.Trace (trace)

import Data.Ix
import Language.Pads.Padsc hiding (position, head)
import Language.Pads.GenPretty
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V
import Constants
import ConstantsGen

ws = REd "[\t ]+|$" " "



[pads|
  data SmurfFile = SmurfFile { header::SmurfHeader, 
                               hmm::HMMp <| (getAlphabet header, 
                                            getNumNodes header) |> }
  
  type SmurfHeader = [Line HeaderLine] terminator Try (LitRE 'HMM ')
  
  data HeaderLine = HeaderLine { tag::Tag, ws, payload::Payload tag }
  
  data Tag = FileVersion "HMMER3/a" -- this string literal will change with 
                                    -- major file version changes
            | NAME | ACC | DESC | LENG | ALPH | RF | CS | MAP | DATE | MEAN 
            | RMSD | COM | NSEQ | EFFN | CKSUM | GA | TC | NC | STATS 
            | BETA | Other (StringSE ws)
  
  data Payload (t::Tag) = case t of
      FileVersion -> Version VersionString
    | NAME -> Name StringLn
    | ACC -> Accession StringLn
    | DESC -> Description StringLn
    | LENG -> ModelLength Int
    | ALPH -> AlphabetP StringLn -- amino or nucleotide
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
    | STATS -> Stats { "LOCAL", ws, 
                       scoredist :: ScoreDistribution, ws, 
                       values :: [Double | ws] terminator Try EOR }
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
  data Exposure = Buried 'i'
                | Exposed 'o'  
                
  data Direction = Parallel "1"
                 | Antiparallel "-1"
  
  data HMMp (alphabet :: String, numNodes :: Int) = 
     HMMp { "HMM"
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
                                   EmissionProbabilitiesP alphabet, ws, EOR)
           -- the insertZeroEmissions and stateZeroTransitions are the
           -- transition probabilities for the BEGIN node of the whole HMM.
           -- match state 0 is the BEGIN state, which is mute so has no
           -- match emission probabilities. the insertZeroEmissions is
           -- the emission probability table for an insert state that may occur
           -- before the first real match state (M1)                                    
           , insertZeroEmissions :: InsertEmissionsP alphabet
           -- the stateZeroTransitions are the transition probabilities for
           -- b->m1, b->i0, b->d1, i0->m1, i0->i0, d0->m1 (always 0.0), d0->d1 (always *)
           -- recall that these are log probabilities, so log1 == 0 and log0 == infinity (*)
           , stateZeroTransitions :: StateTransitionsP
           -- the "regular" nodes, which are the rest of the state transition and emission
           -- probabilities. See the HmmNode type for full documentation
           , nodes :: [HmmNodeP <| alphabet |>] terminator "//" 
             where <| numNodes == length nodes |>
           }
              
  type Letter (alphabet :: String) = constrain c :: Char 
                                     where <| c `elem` alphabet |>
  
  type EmissionProbabilitiesP (alphabet :: String) = 
       [ Double | ws ] length <| (length alphabet) - numAlphabetAdditions |> 

  -- A bit of a hack here, because Pads doesn't yet support non-base types
  -- as the parameteter for an algebraic parser type
  -- note that 0 is for a match, 1 for insertion, and 2 for a deletion
  -- 
  data TransitionProbabilitiesP = TransitionProbabilitiesP {
      m_mP :: PTransitionProbability <|(0, 0)|>, ws,
      m_iP :: PTransitionProbability <|(0, 1)|>, ws,
      m_dP :: PTransitionProbability <|(0, 2)|>, ws,
      i_mP :: PTransitionProbability <|(1, 0)|>, ws,
      i_iP :: PTransitionProbability <|(1, 1)|>, ws,
      d_mP :: PTransitionProbability <|(2, 0)|>, ws,
      d_dP :: PTransitionProbability <|(2, 2)|>
  }
  
  type TransitionDescription = [ StringSE ws | ws] terminator (Try EOR)
  
  type InsertEmissionsP (alphabet :: String) = 
       (ws, EmissionProbabilitiesP alphabet, ws, EOR)
  
  type StateTransitionsP = (ws, TransitionProbabilitiesP, ws, EOR)
  
  data HmmNodeP (alphabet::String) = 
       HmmNodeP { ws
               -- simply the index (number) of the node. Begin state is 0.
               , nodeNumP :: Int
               , ws
               -- Emission log-odds probabilities for the match state
               -- remember these are mapped to the alphabet in alphabetic order
               , matchEmissionsP :: EmissionProbabilitiesP alphabet
               , ws
               -- these are three extra fields for MAP, RF, and CS
               -- we do not use them in the Smurf2 algorithm; see the
               -- HMMER3 user guide if you are curious.
               , annotationsP :: Maybe EmissionAnnotationSet
               , EOR
               -- these fields are the insert emission log-odds scores, one per
               -- symbol in alphabetic order
               , insertionEmissionsP :: InsertEmissionsP alphabet
               -- these fields are the transition log-odds for this node in order:
               -- mk->(mk+1, ik, dk+1), ik->(mk+1, ik); dk->(mk+1, dk+1)
               -- note that these correspond exactly to the edges in a profile
               -- HMM state-transition diagram
               , transitionsP :: StateTransitionsP
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
                      

  data PTransitionProbability (fState::PHMMState, tState::PHMMState) = 
       PTransitionProbability { pLogProbability :: LogProbability
                              , pFromState = value fState :: PHMMState
                              , pToState = value tState :: PHMMState
                              }

  type PHMMState = Int
                      
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

-- may need to use EtoC + CtoT for M_E and NtoB for begin, for local-local,
-- but SMURF sets them to zero.

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

type StateAcc = TransitionProbabilities -> TransitionProbability

getAlphabet :: SmurfHeader -> String
getAlphabet ((HeaderLine {tag, payload}):xs) = case tag of
                    ALPH -> case payload of
                              AlphabetP "amino" -> V.toList amino
                              AlphabetP "nucleotide" -> V.toList nucleotide
                              otherwise -> error "Invalid alphabet"
                    otherwise -> getAlphabet xs

getNumNodes :: SmurfHeader -> Int
getNumNodes ((HeaderLine {tag, payload}):xs) = case tag of
                    LENG -> case payload of
                              ModelLength i -> i
                              otherwise -> error "Invalid model length"
                    otherwise -> getNumNodes xs

type HMM = V.Vector HmmNode

type EmissionProbabilities = V.Vector Double
type InsertEmissions = EmissionProbabilities

-- See type definition for 'HmmNodeP' above for some documentation.
-- The purpose of re-creating the HmmNode type is to add occupy
-- probabilities to each HmmNode as a pre-processing step.
data HmmNode = 
     HmmNode { nodeNum :: Int
             , matchEmissions :: EmissionProbabilities
             , annotations :: Maybe EmissionAnnotationSet
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

data TransitionProbability = 
     TransitionProbability { logProbability :: LogProbability
                           , fromState :: HMMState
                           , toState :: HMMState
                           } deriving (Show)

-- I don't think this is completely needed, but I feel more comfortable
-- mirroring the Pads types at the moment.
type StateTransitions = TransitionProbabilities
                    
getHmmNodes :: HMMp -> HMM
getHmmNodes hmm = divOccSum
  where z = HmmNodeP { nodeNumP = 0 
                     , matchEmissionsP = (replicate (V.length amino) maxProb) 
                     , annotationsP = Nothing 
                     , insertionEmissionsP = (insertZeroEmissions hmm) 
                     , transitionsP = (stateZeroTransitions hmm) 
                     }

        divOccSum :: HMM
        divOccSum = V.map divBySum withOccupy

        divBySum :: HmmNode -> HmmNode
        divBySum node = node { transitions = otran { b_m = newBM } }
          where otran = transitions node
                newBM = mkDefTransProb newOccProb Beg Mat
                newOccProb = NonZero $ 
                               (-(log (exp (-(logst b_m node)) / occSum)))

        occSum :: Double
        occSum = V.ifoldl addOcc 0.0 withOccupy

        addOcc :: Double -> Int -> HmmNode -> Double
        addOcc sum i node = sum + (occProb * 
                                    (fromIntegral ((V.length withOccupy) - i)))
          where occProb = exp (-(logst b_m node))

        withOccupy :: HMM
        withOccupy = snd 
                     $ foldl addOccupy (0, V.empty) 
                     $ snd $ foldr addMatchEnd (0, [])
                     $ map convert (z:nodes hmm)

        convert :: HmmNodeP -> HmmNode
        convert n = HmmNode { nodeNum = nodeNumP n
                            , matchEmissions = V.fromList $ convertEmissions $ matchEmissionsP n
                            , annotations = annotationsP n
                            , insertionEmissions = V.fromList $ convertEmissions $ insertionEmissionsP n
                            , transitions = newTrans
                            }
          where otran = transitionsP n
                m_eDef = mkDefTransProb LogZero Mat End
                b_mDef = mkDefTransProb LogZero Beg Mat
                newTrans = TransitionProbabilities { m_m = convTrans $ m_mP otran
                                                   , m_i = convTrans $ m_iP otran
                                                   , m_d = convTrans $ m_dP otran
                                                   , i_m = convTrans $ i_mP otran
                                                   , i_i = convTrans $ i_iP otran
                                                   , d_m = convTrans $ d_mP otran
                                                   , d_d = convTrans $ d_dP otran
                                                   , b_m = b_mDef
                                                   , m_e = m_eDef
                                                   }
                convertEmissions emissions = emissions ++ [-(log xEmission)]
                  where xEmission = foldr (+) 0 (map xEmission' (zip (V.toList bgFreqs) emissions))
                        xEmission' (f, e)= f * exp (- e)

        convTrans :: PTransitionProbability -> TransitionProbability
        convTrans ptrans =
          TransitionProbability { logProbability = pLogProbability ptrans
                                , fromState = intToAlg $ pFromState ptrans
                                , toState = intToAlg $ pToState ptrans
                                }
          where intToAlg state
                  | state == 0 = Mat
                  | state == 1 = Ins
                  | state == 2 = Del
                  | state == 3 = Beg
                  | state == 4 = End
                  | state == 5 = BMat

        mkDefTransProb :: LogProbability -> HMMState -> HMMState 
                          -> TransitionProbability
        mkDefTransProb logProb f t = 
          TransitionProbability { logProbability = logProb
                                , fromState = f
                                , toState = t
                                }

        -- in log space:
        -- M_E_n = 0, S_n = 0
        -- M_E_k = M_D_k + S_k+1, S_k = S_k+1 + D_D_k
        addMatchEnd :: HmmNode -> (Double, [HmmNode]) -> (Double, [HmmNode])
        addMatchEnd n (sk, nodes) = (sk', n':nodes)
          where n' = n { transitions = otran { m_e = mkDefTransProb mek Mat End } }
                otran = transitions n
                mek = if null nodes then 
                        NonZero 0
                      else 
                        NonZero $ logst m_d n + sk
                sk' = if null nodes then 0 else sk + logst d_d n

        -- The key point of this folding function is to *use the accumulator*
        -- to calculate the occupy probability for the current node.
        -- In this case, the accumulator is a vector containing all of the
        -- nodes.
        --
        -- This function uses 'snoc', which appends elements to the end of a
        -- vector. Its complexity is O(n) where n is the size of the vector.
        --
        -- mocc_0 = M_I_0 + M_M_0
        -- mocc_k = mocc_k-1 * (M_M_k-1 + M_I_k-1) + D_M_k-1 * (1 - mocc_k-1)
        --
        -- Remember that the above is in probability space. So for each term
        -- in the above equations X, e^-X should be applied before calculation.
        -- Once the calculation is done, -ln(result) should be used to bring
        -- things back to log space.
        addOccupy :: (Int, V.Vector HmmNode) -> HmmNode
                     -> (Int, V.Vector HmmNode)
        addOccupy (i, nodes) n = ( i + 1
                                 , V.snoc nodes n'
                                 )
          where n' = n { transitions = ntran }
                ntran = otran { b_m = mkDefTransProb occProb Beg Mat }
                otran = transitions n
                occProb
                  | i == 0 = NonZero $ 
                               -(log (exp (-(logst m_i n))
                               + exp (-(logst m_m n))))
                  | otherwise = let pmocc = exp (-prevMocc)
                                    pm_m = exp (-(logst m_m pnode))
                                    pm_i = exp (-(logst m_i pnode))
                                    pd_m = exp (-(logst d_m pnode))
                                    mocc = pmocc 
                                           * (pm_m + pm_i) + pd_m * (1 - pmocc)
                                in  NonZero (-(log mocc))

                pnode = nodes V.! (i - 1)

                prevMocc :: Double
                prevMocc = getlog $ logProbability $ b_m $ transitions pnode

        logst :: StateAcc -> HmmNode -> Double                
        logst st node = getlog $ logProbability $ st $ transitions node

        getlog :: LogProbability -> Double
        getlog lp = case lp of
                      LogZero -> error "cannot compute with log zero"
                      NonZero d -> d

getTag :: Tag -> SmurfHeader -> Payload
getTag t ((HeaderLine {tag, payload}):xs) = if tag == t
                                              then payload
                                            else
                                              getTag t xs
getTag t [] = error "Tag not found"  

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
             return (header, getHmmNodes hmm, md)

