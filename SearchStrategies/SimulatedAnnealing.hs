module SearchStrategies.SimulatedAnnealing where

import qualified Data.Vector.Unboxed as V
import System.Random (mkStdGen, random, StdGen)

import Debug.Trace (trace)

import Beta
import HMMPlus
import MRFTypes
import Score
import SearchModel
import SearchStrategy
import StochasticSearch
import Viterbi

import qualified SearchStrategies.RandomHillClimb as RHC

nss :: NewSS
nss hmm searchP query betas =
      SS { gen0    = \seed -> RHC.initialize hmm searchP seed query betas 
         , nextGen = RHC.mutate searchP query betas 
         , accept  = histProgresses (bolzmannProgress searchP)
         , quit    = RHC.terminate searchP 
         }

bolzmannProgress :: SearchParameters -> Seed -> SearchDelta a -> Bool
bolzmannProgress searchP seed delta = ok (younger delta) (older delta)
  where ok p1 p2 = boltzmann (youngerAge delta) (scoreOf p1) (scoreOf p2) >= uniform
        boltzmann :: Age -> Score -> Score -> Double
        boltzmann age (Score s1) (Score s2) = exp ((-(s1 - s2)) 
                                       / (constBoltzmann * temperature))
          where temperature = (constCooling ^^ age) * constInitTemp
                
        uniform :: Double
        uniform = (fst . random . mkStdGen) seed --- XXX horror show
                
        constBoltzmann = getSearchParm searchP boltzmannConstant
        constInitTemp = getSearchParm searchP initialTemperature
        constCooling = getSearchParm searchP coolingFactor
