module SearchStrategies.SimulatedAnnealing where

import qualified Data.Vector as V
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
      SS { gen0    = \seed -> RHC.initialize' hmm searchP seed query betas 
         , nextGen = RHC.mutate' searchP query betas 
         , accept  = histProgresses (bolzmannProgress searchP)
         , quit    = RHC.terminate' searchP 
         }

bolzmannProgress :: SearchParameters -> Seed -> ShortHistory a -> Bool
bolzmannProgress searchP seed sh = ok (younger sh) (older sh)
  where ok (p1, age) (p2, _) = boltzmann (scoreOf p1) (scoreOf p2) >= uniform
          where uniform :: Double
                uniform = (fst . random . mkStdGen) seed --- XXX horror show
                
                boltzmann :: Score -> Score -> Double
                boltzmann (Score s1) (Score s2) = exp ((-(s1 - s2)) 
                                       / (constBoltzmann * temperature))
                
                temperature = (constCooling ^^ age) * constInitTemp
                
                constBoltzmann = getSearchParm searchP boltzmannConstant
                constInitTemp = getSearchParm searchP initialTemperature
                constCooling = getSearchParm searchP coolingFactor
