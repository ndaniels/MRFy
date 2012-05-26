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

nss :: HMM -> SearchParameters -> QuerySequence -> [BetaStrand] -> SearchStrategy Placement
nss hmm searchP query betas =
      SS { gen0 = \seed -> RHC.initialize' hmm searchP seed query betas 
         , nextGen = \seed scorer placements ->
                      RHC.mutate' searchP query betas seed scorer placements
         , accept = \seed hist age ->
                     accept' searchP seed hist age
         , quit = \hist age ->
                   RHC.terminate' searchP hist age
         }

accept' :: SearchParameters -> Seed -> [Scored Age] -> Age -> Bool
accept' _ _ [] _ = error "go away"
accept' _ _ [s] _ = True
accept' searchP seed (a1:a2:_) age = boltzmann s1 s2 >= (p :: Double)
  where (s1, s2) = (unScore $ scoreOf a1, unScore $ scoreOf a2)
        (p, gen) = random (mkStdGen seed)
    
        boltzmann :: Double -> Double -> Double
        boltzmann s1 s2 = exp ((-(s1 - s2)) 
                               / (constBoltzmann * temperature))

        temperature = (constCooling ^^ age) * constInitTemp

        constBoltzmann = getSearchParm searchP boltzmannConstant
        constInitTemp = getSearchParm searchP initialTemperature
        constCooling = getSearchParm searchP coolingFactor

