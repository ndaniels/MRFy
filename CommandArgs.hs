module CommandArgs where

import StochasticSearch

-- import qualified SearchStrategies as SS 
import qualified SearchStrategies.GeneticAlgorithm as GeneticAlgorithm
import qualified SearchStrategies.RandomHillClimb as RandomHillClimb
import qualified SearchStrategies.SimulatedAnnealing as SimulatedAnnealing

searchP = SearchParameters { strategy = SimulatedAnnealing.ss
                           , generations = 1000
                           , multiStartPopSize = 10
                           , populationSize = Just 2000
                           , initialTemperature = Just 1000.0
                           , coolingFactor = Just 0.99
                           , boltzmannConstant = Just 1.0
                           , mutationRate = Just 1.0
                           }

