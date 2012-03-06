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

accept' :: SearchParameters -> Seed -> History -> Age -> Bool
accept' _ _ [] _ = error "go away"
accept' _ _ [(s1, _)] _ = True
accept' searchP seed ((s1, _):(s2, _):scores) age = boltzmann s1 s2 >= (p :: Double)
  where (p, gen) = random (mkStdGen seed)
    
        boltzmann :: Score -> Score -> Double
        boltzmann s1 s2 = exp ((-(s1 - s2)) 
                               / (constBoltzmann * temperature))

        temperature = (constCooling ^^ age) * constInitTemp

        constBoltzmann = getSearchParm searchP boltzmannConstant
        constInitTemp = getSearchParm searchP initialTemperature
        constCooling = getSearchParm searchP coolingFactor

