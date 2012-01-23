module SearchStrategies.SimulatedAnnealing where

import qualified Data.Vector as V
import System.Random (mkStdGen, random, StdGen)

import Debug.Trace (trace)

import Beta
import SearchStrategy
import StochasticSearch
import Viterbi

import qualified SearchStrategies.RandomHillClimb as RHC

ss :: SearchStrategy
ss = SearchStrategy { accept = accept'
                    , terminate = terminate RHC.ss
                    , mutate = mutate RHC.ss
                    , initialize = initialize RHC.ss
                    }

accept' :: Seed -> [Score] -> Age -> Bool
accept' _ [] _ = error "go away"
accept' _ [s1] _ = True
accept' seed (s1:s2:scores) age = boltzmann s1 s2 >= (p :: Double)
  where (p, gen) = random (mkStdGen seed)
    
        boltzmann :: Score -> Score -> Double
        boltzmann s1 s2 = exp ((-(s1 - s2)) / (k * temperature))

        k = 1.0
        temperature = (0.9 ^^ age) * initialTemperature
        initialTemperature = 1000.0

