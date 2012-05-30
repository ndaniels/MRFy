module CommandArgs where

import System.Console.GetOpt
import Data.Maybe (fromMaybe)

import StochasticSearch

-- import qualified SearchStrategies as SS 
import qualified SearchStrategies.GeneticAlgorithm as GeneticAlgorithm
import qualified SearchStrategies.RandomHillClimb as RandomHillClimb
import qualified SearchStrategies.SimulatedAnnealing as SimulatedAnnealing

data Flag = Verbose | Version | Help
            | StratSA | StratGA | StratRand
            | Generations String | MultiStartPop String
            | PopSize String | InitTemp String | CoolingFact String
            | BoltzmannConst String | MutationRate String | Convergence String

options :: [OptDescr Flag]
options =
  [ Option ['v'] ["verbose"] (NoArg Verbose) "chatty output"
  , Option ['V'] ["version"] (NoArg Version) "show version number"
  , Option ['h'] ["help"] (NoArg Help) "show help"
  , Option [] ["gens"] (ReqArg Generations "INT") "set the number of generations"
  , Option [] ["multipop"] (ReqArg MultiStartPop "INT") "set the size of each multi start population"
  , Option [] ["popsize"] (ReqArg PopSize "INT") "set the size of the population"
  , Option [] ["inittemp"] (ReqArg InitTemp "DOUBLE") "set initial temperature for SA"
  , Option [] ["coolfact"] (ReqArg CoolingFact "DOUBLE") "set cooling factor for SA"
  , Option [] ["boltz"] (ReqArg BoltzmannConst "DOUBLE") "set the boltzmann constant"
  , Option [] ["mutrate"] (ReqArg MutationRate "DOUBLE") "set the mutation rate"
  , Option [] ["convergence"] (ReqArg Convergence "INT") "set num generations before convergence"
  , Option ['s'] ["simanneal"] (NoArg StratSA) "use simulated annealing"
  , Option ['g'] ["genetic"] (NoArg StratGA) "use genetic algorithms"
  , Option ['r'] ["random"] (NoArg StratRand) "use random hill climbing"
  -- , Option ['p'] [] (ReqArg HmmPlusFile) "input hmm plus file" 
  -- , Option ['f'] [] (ReqArg FastaFile) "input fasta file" 
  -- , Option ['o'] ["output"] (OptArg (\s -> OutputFile (fromMaybe "stdout"))) "output file" 
  ]

data Files = Files { hmmPlusF :: String
                   , fastaF :: String
                   , outputF :: String
                   }

-- | Things this program can be commanded to do
data Commanded = AlignmentSearch SearchParameters Files
               | TestHmm String
     

getFiles :: [String] -> Files
getFiles [hmmPlus, fasta] = Files { hmmPlusF = hmmPlus
                                  , fastaF = fasta
                                  , outputF = "stdout"
                                  }
getFiles [hmmPlus, fasta, output] = Files { hmmPlusF = hmmPlus
                                          , fastaF = fasta
                                          , outputF = output
                                          }
getFiles _ = error "AG fool"

getParams :: [Flag] -> SearchParameters
getParams [] = defaultSP
getParams (f:fs) =
  case f of
    Verbose -> params { verbose = True }
    Version -> error "show version number"
    Help -> error "show help"
    StratSA -> params { strategy = SimulatedAnnealing.nss }
    StratGA -> params { strategy = GeneticAlgorithm.nss }
    StratRand -> params { strategy = RandomHillClimb.nss }
    Generations x -> params { generations = read x }
    MultiStartPop x -> params { multiStartPopSize = read x }
    PopSize x -> params { populationSize = Just $ read x }
    InitTemp x -> params { initialTemperature = Just $ read x }
    CoolingFact x -> params { coolingFactor = Just $ read x }
    BoltzmannConst x -> params { boltzmannConstant = Just $ read x }
    MutationRate x -> params { mutationRate = Just $ read x }
    Convergence x -> params { convergenceAge = Just $ read x }
  where params = getParams fs


getOpts :: [String] -> Commanded 
getOpts ["-test", what] = TestHmm what
getOpts argv =
    case getOpt RequireOrder options argv of
      (o, moreArgs, []) -> AlignmentSearch (getParams o)  (getFiles moreArgs)
      (_, _, errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: mrfy [OPTION ...] files..."

defaultSP = SearchParameters { strategy = SimulatedAnnealing.nss
                             , generations = 1000
                             , multiStartPopSize = 10
                             , verbose = True
                             , populationSize = Just 20
                             , initialTemperature = Just 1000.0
                             , coolingFactor = Just 0.99
                             , boltzmannConstant = Just 1.0
                             , mutationRate = Just 1.0
                             , convergenceAge = Nothing
                             , secPreds = Nothing
                             }
