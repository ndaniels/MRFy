module CommandArgs where

import StochasticSearch

import qualified SearchStrategies as SS

searchP = SearchParameters { strategy = SS.SimulatedAnnealing.ss
                           , generations = 1000
                           , populationSize = 1
                           , initialTemperature = Just 1000.0
                           , coolingFactor = Just 0.99
                           , boltzmannConstant = Just 1.0
                           , mutationRate = Just 1.0
                           }

