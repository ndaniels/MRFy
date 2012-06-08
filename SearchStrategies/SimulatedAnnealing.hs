{-# LANGUAGE NamedFieldPuns #-}
module SearchStrategies.SimulatedAnnealing where

import Control.Monad.Random
import qualified Data.Vector.Unboxed as V

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
nss hmm searchP query betas scorer = fullSearchStrategy
  (fmap scorer $ RHC.initialize hmm searchP query betas)
  (RHC.mutate searchP query betas scorer)
  (boltzmannUtility searchP)
  (takeByAgeGap (acceptableAgeGap searchP))
  id

boltzmannUtility
  :: RandomGen r => SearchParameters -> SearchDelta a -> Rand r (Utility a)
boltzmannUtility searchP (SearchDelta { younger, older, youngerAge }) = do
  uniform <- getRandom
  return $ if boltzmann youngerAge (scoreOf younger) (scoreOf older) >= uniform
           then Useful (unScored younger)
           else Useless
  where boltzmann :: Age -> Score -> Score -> Double
        boltzmann age (Score s1) (Score s2) =
          exp (negate (s1 - s2) / (constBoltzmann * temperature))
          where temperature = (constCooling ^^ age) * constInitTemp
                
        constBoltzmann = getSearchParm searchP boltzmannConstant
        constInitTemp = getSearchParm searchP initialTemperature
        constCooling = getSearchParm searchP coolingFactor
