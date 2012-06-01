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
      SS { gen0 = \seed -> RHC.initialize' hmm searchP seed query betas 
         , nextGen = \seed scorer placements ->
                      RHC.mutate' searchP query betas seed scorer placements
         , accept = \seed hist age ->
                     accept' searchP seed hist age
         , quit = \hist age ->
                   RHC.terminate' searchP hist age
         }

accept' :: SearchParameters -> Seed -> History Placement -> Age -> Bool
accept' searchP seed (History ps) age = ok ps
  where ok []        = error "empty history passed to accept predicate"
        ok [s1]      = True
        ok ((p1,a1):(p2,a2):_) = boltzmann s1 s2 >= (p :: Double)
--accept' searchP seed (a1:a2:_) age = boltzmann s1 s2 >= (p :: Double)
          where 
                (s1, s2) = (unScore $ scoreOf p1, unScore $ scoreOf p2)
                (p, gen) = random (mkStdGen seed)
                
                boltzmann :: Double -> Double -> Double
                boltzmann s1 s2 = exp ((-(s1 - s2)) 
                                       / (constBoltzmann * temperature))
                
                temperature = (constCooling ^^ age) * constInitTemp
                
                constBoltzmann = getSearchParm searchP boltzmannConstant
                constInitTemp = getSearchParm searchP initialTemperature
                constCooling = getSearchParm searchP coolingFactor

