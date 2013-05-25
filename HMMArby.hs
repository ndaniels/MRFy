{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module HMMArby where

import Control.Applicative
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random
import Test.QuickCheck hiding (ShrinkState)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
-- import Data.List 

import Beta
import Constants
import HMMPlus
import MRFTypes
import qualified SearchStrategies.GeneticAlgorithm as GA
import Score
import StochasticSearch
import Viterbi

import Debug.Trace (trace)

verifySearchGuess :: HMM -> [BetaStrand] -> Placement -> Bool
verifySearchGuess _ [] [] = True
verifySearchGuess _ [] [g] = False
verifySearchGuess _ [b] [] = False
verifySearchGuess hmm (b:bs) (g:gs) = range && noClash
  where range = g >= 0 && (g + len b) <= V.length hmm
        noClash = case gs of
                    [] -> True
                    (g':gs') -> g' > g + len b

instance Arbitrary QuerySequence where
  shrink = (map U.fromList) . (shrink . U.toList)
  arbitrary = do
    qs <- arbitrary
    return $ U.fromList $ take (min (length qs) 12) qs

instance Arbitrary AA where
  arbitrary = arbitraryBoundedIntegral

prob :: Gen Double
prob = choose (0.0, 1.0)

minimumModelSize :: Int
minimumModelSize = 2

instance Arbitrary (V.Vector HMMNode) where
  shrink = map V.fromList . filter ((>= minimumModelSize) . length) . shrink . V.toList
  arbitrary = do
    -- A random length is used here to essentially guarantee
    -- that we get a list of HMMNodes with at least length 2.
    -- Is there a better way to do this? (Preferably without
    -- setting a maximum.)
    randLen <- sized $ \n -> choose (minimumModelSize, n `max` minimumModelSize)
    nodes <- vectorOf randLen arbitrary :: Gen [HMMNode]
    return $ V.fromList $ map annotate $ zip [0..] nodes

    -- Add incrementing node numbers to [HMMNode].
    where annotate (i, node) = node { nodeNum = i }

-- Make node numbers valid. I think that should be
-- done in `Arbitrary (V.Vector HMMNode)`.
instance Arbitrary HMMNode where
  arbitrary = do
    -- (rMatEmi, rInsEmi) <- fmap unzip $ listOf1 (liftM2 (,) prob prob) 
    rMatEmi <- sequence $ take (length aminoList) $ repeat prob
    rInsEmi <- sequence $ take (length aminoList) $ repeat prob
    rTrans <- arbitrary :: Gen TScores
    return HMMNode { nodeNum = 0
                   , matEmissions = toScoreVec rMatEmi
                   , insEmissions = toScoreVec rInsEmi
                   , transitions  = rTrans
                   }
    where toScoreVec = U.fromList . map toScore
  shrink n = [n { matEmissions = m } | m <- shrinkE (matEmissions n)] ++
             [n { insEmissions = i } | i <- shrinkE (insEmissions n)] ++
             [n { transitions  = t } | t <- shrinkT clobber (transitions n)]
    where clobber p = if p < negLogZero then Just negLogZero else Nothing
instance Arbitrary TScores where
  arbitrary = mkTransScores <$> p <*> p <*> p <*> p <*> p <*> p <*> p
   where p = arbitrary   

instance Arbitrary Score where
  arbitrary = do
    f <- choose (0.0, 1.0) :: Gen Double
    return $ if f == 0.0 then negLogZero
             else Score $ - log f

instance Arbitrary StateLabel where
  -- Test this with sample (arbitrary :: Gen StateLabel)
  arbitrary = elements [minBound..maxBound]

-- | Map a probability to a score
toScore :: Double -> Score
toScore f = if f == 0.0 then negLogZero else Score (- log f)
 


{-
Think of a graymap image.
The image triggers a bug in my program.
I want the simplest possible image.
That means as many white pixels as possible.
So in an ideal world, each shrinking step would turn half the nonwhite pixels white.
That's hard to implement, so I'm turning just one pixel white.
The white pixel corresponds to probability zero.
As long as it is not white, there is no point changing its color.
-}

shrinkE :: EScores -> [EScores]
-- push probabilities to zero.  Will be *very* slow.
-- (a good person would shrink half the probs at every go)
shrinkE v = [smash i | i <- [0..U.length v - 1], v U.! i < negLogZero ]
    where smash i = v U.// [(i, negLogZero)]

shrinkT :: (Score -> Maybe Score) -> TScores -> [TScores]
shrinkT shrinkOne t = onlyShrunk $
  do new_m_m <- hit $ m_m t
     new_m_i <- hit $ m_i t
     new_m_d <- hit $ m_d t
     new_d_m <- hit $ d_m t
     new_d_d <- hit $ d_d t
     new_i_m <- hit $ i_m t
     new_i_i <- hit $ i_i t
     return $ TScores { m_m = new_m_m
                      , m_i = new_m_i
                      , m_d = new_m_d
                      , d_m = new_d_m
                      , d_d = new_d_d
                      , i_m = new_i_m
                      , i_i = new_i_i
                      }
  where hit p = shrinkMe shrinkOne p
        
onlyShrunk :: ShrinkMonad a -> [a]
onlyShrunk (SM ss) = [a | Shrunk a <- ss]

data ShrunkScore = Maximized | Untouched Score
data ShrinkState a = Shrunk a | Waiting a
data ShrinkMonad a = SM { smStates :: [ShrinkState a] }
instance Monad ShrinkMonad where
  return a = SM [Waiting a]
  SM ss >>= k = SM $ concatMap (combine k) ss
    where combine k (Shrunk a)  = [Shrunk b | Waiting b <- smStates (k a)]
          combine k (Waiting a) = smStates $ k a

shrinkMe :: (a -> Maybe a) -> a -> ShrinkMonad a
shrinkMe shrink a =
  case shrink a of Nothing -> return a
                   Just shrunk -> SM [Waiting a, Shrunk shrunk]
                    
runShrink ss = [a | Shrunk a <- ss]




-- possibly useful idea

class Probabilities a where
    normalize :: a -> a  -- normalize a group of probabilities

instance Probabilities (Double, Double, Double) where
    normalize (x, y, z) = if sum == 0.0 then (x, y, z)
                          else (x / sum, y / sum, z / sum)
      where sum = x + y + z

instance Probabilities (Double, Double) where
    normalize (x, y) = if sum == 0.0 then (x, y)
                          else (x / sum, y / sum)
      where sum = x + y

instance Probabilities (V.Vector Double) where
  normalize xs = if sum == 0.0 then xs
                 else V.map (/ sum) xs
    where sum = V.foldl' (+) 0.0 xs
