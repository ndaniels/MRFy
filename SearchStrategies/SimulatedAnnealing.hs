{-# LANGUAGE NamedFieldPuns #-}
module SearchStrategies.SimulatedAnnealing where

import qualified Data.Vector.Unboxed as V
import System.Random (mkStdGen, random, StdGen)

import Debug.Trace (trace)

import Beta
import HMMPlus
import LazySearchModel
import MRFTypes
import Score
import SearchStrategy
import StochasticSearch
import Viterbi

import qualified SearchStrategies.RandomHillClimb as RHC

nss :: NewSS
nss hmm searchP query betas scorer = searchStrategy
  (\seed -> map scorer $ RHC.initialize hmm searchP seed query betas)
  (RHC.mutate searchP query betas scorer)
  (boltzmannUtility searchP)
  (takeByAgeGap (acceptableAgeGap searchP))

boltzmannUtility :: SearchParameters -> Seed -> SearchDelta a -> Utility a
boltzmannUtility searchP seed (SearchDelta { younger, older, youngerAge }) =
  if boltzmann youngerAge (scoreOf younger) (scoreOf older) >= uniform
  then Useful (unScored younger)
  else Useless
  where boltzmann :: Age -> Score -> Score -> Double
        boltzmann age (Score s1) (Score s2) =
          exp (negate (s1 - s2) / (constBoltzmann * temperature))
          where temperature = (constCooling ^^ age) * constInitTemp
                
        uniform :: Double
        uniform = (fst . random . mkStdGen) seed
                
        constBoltzmann = getSearchParm searchP boltzmannConstant
        constInitTemp = getSearchParm searchP initialTemperature
        constCooling = getSearchParm searchP coolingFactor
