{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module SearchStrategies.SimulatedAnnealing where

import Control.Monad.LazyRandom

import LazySearchModel
import Score
import SearchStrategy
import StochasticSearch

import qualified SearchStrategies.RandomHillClimb as RHC

nss :: NewSS
nss hmm searchP query betas scorer = fullSearchStrategy
  (fmap scorer $ RHC.initialize hmm searchP query betas)
  (RHC.mutate searchP query betas scorer)
  (boltzmannUtility searchP)
  (takeByCCostGap (acceptableCCostGap searchP))
  id

boltzmannUtility
  :: RandomGen r => SearchParameters -> Move a -> Rand r (Utility (Scored a))
boltzmannUtility searchP (Move { younger, older, youngerCCost }) = do
  uniform <- getRandom
  return $ if boltzmann youngerCCost (scoreOf younger) (scoreOf older) >= uniform
           then Useful younger
           else Useless
  where boltzmann :: CCost -> Score -> Score -> Double
        boltzmann cost (Score youngScore) (Score oldScore) =
          exp ((oldScore - youngScore) / (constBoltzmann * temperature))
          where temperature = (constCooling ^^ cost) * constInitTemp
                
        constBoltzmann = getSearchParm searchP boltzmannConstant
        constInitTemp = getSearchParm searchP initialTemperature
        constCooling = getSearchParm searchP coolingFactor
