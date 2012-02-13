module CommandArgs where

import StochasticSearch

-- import qualified SearchStrategies as SS 
import qualified SearchStrategies.RandomHillClimb as RandomHillClimb
import qualified SearchStrategies.SimulatedAnnealing as SimulatedAnnealing

searchP = SearchParameters { strategy = SimulatedAnnealing.ss
                           , generations = 1000
                           , populationSize = 20
                           , initialTemperature = Just 1000.0
                           , coolingFactor = Just 0.99
                           , boltzmannConstant = Just 1.0
                           , mutationRate = Just 1.0
                           }

