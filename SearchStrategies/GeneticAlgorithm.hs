{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module SearchStrategies.GeneticAlgorithm where

import Control.Applicative
import Control.Monad.LazyRandom
import Data.List
import qualified Data.Vector.Unboxed as V

import Beta
import MRFTypes
import ParRandom
import Score
import SearchStrategy 
import SearchStrategies.RandomHillClimb (randomizePlacement)
import LazySearchModel
import Shuffle
import StochasticSearch
import Viterbi

wrapBestScore :: [Scored a] -> Scored [Scored a]
wrapBestScore as = Scored as (scoreOf $ minimum as)


nss :: NewSS
nss hmm searchP query betas scorer = fullSearchStrategy
  (fmap (wrapBestScore . map scorer) $ initialize hmm searchP query betas)
  (mutate searchP query betas scorer)
  scoreUtility
  (takeNGenerations (generations searchP))
  (unScored . minimum)

type Population = [Scored Placement]

initialize
  :: RandomGen r
  => HMM -> SearchParameters -> QuerySequence -> [BetaStrand] -> Rand r [Placement]
initialize hmm searchP query betas = 
  sequence $ take n $ repeat $ projInitialGuess hmm (getSecPreds searchP) query betas
  where n = getSearchParm searchP populationSize

-- invariant: len [SearchSolution] == 1
mutate :: SearchParameters
       -> QuerySequence
       -> [BetaStrand]
       -> Scorer Placement
       -> Scored Population
       -> Rand StdGen (Scored Population)
mutate searchP query betas scorer (Scored placements _) = wrapBestScore <$> fittest
  where fittest = do gen <- getSplit
                     ( fst
                       . shuffle gen
                       . take (getSearchParm searchP populationSize)
                       . sort
                       . (placements ++)
                       ) <$> progeny
        progeny = parRandom $ map (\gs -> scorer <$>
                                          randomizePlacement betas gs (V.length query))
                            $ getPairings
                            $ map unScored placements

getPairings :: [Placement] -> [Placement]
getPairings [] = []
getPairings [p] = [p]
getPairings (p1:p2:ps) = crossover p1 p2 : getPairings ps

crossover :: Placement -> Placement -> Placement
crossover ps qs = sort $ crossover' ps qs

-- precondition: length ps == length qs
crossover' :: Placement -> Placement -> Placement
crossover' [] [] = []
crossover' [p] [q] = if p < q then [p] else [q]
crossover' (p:ps) (q:qs) = leftmost:rightmost:crossover' (init ps) (init qs)
  where leftmost = if p < q then p else q
        rightmost = if lastp > lastq then lastp else lastq
        lastp = last ps
        lastq = last qs
crossover' [] (_:_) = error "crossover precondition violated"
crossover' (_:_) [] = error "crossover precondition violated"

