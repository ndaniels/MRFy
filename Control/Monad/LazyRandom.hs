{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Control.Monad.LazyRandom 
  (
    module System.Random,
    module Control.Monad.LazyRandom.Class,
    evalRand,
    runRand,
    evalRandIO,
--    fromList,
    Rand -- but not the data constructors
    )
where
  
import Control.Monad.LazyRandom.Class

--------------------------------------------------------------------------
-- imports

import System.Random
  ( Random(..)
  , RandomGen(..)
  , StdGen, mkStdGen, getStdRandom
  )

import Control.Monad
  ( liftM
  , ap
  )

import Control.Applicative
  ( Applicative(..)
  )

--------------------------------------------------------------------------
-- ** Based on quickcheck generator type

newtype Rand r a = MkRand{ unRand :: r -> a }

instance Functor (Rand r) where
  fmap f (MkRand h) = MkRand (f . h)

instance RandomGen r => Applicative (Rand r) where
  pure  = return
  (<*>) = ap

instance RandomGen r => Monad (Rand r) where
  return x = MkRand (const x)
  
  MkRand m >>= k =
    MkRand (\r ->
      let (r1,r2)  = split r
          MkRand m' = k (m r1)
       in m' r2
    )



-- | Evaluate a random computation using the generator @g@.  Note that the
-- generator @g@ is not returned, so there's no way to recover the
-- updated version of @g@.
evalRand :: (RandomGen g) => Rand g a -> g -> a
evalRand (MkRand f) g = f g
 
-- | Run a random computation using the generator @g@, returning the result
-- and the updated generator.
runRand :: (RandomGen g) => Rand g a -> g -> (a, g)
runRand (MkRand f) g = (f g1, g2)
  where (g1, g2) = split g
 
-- | Evaluate a random computation in the IO monad, using the random number
-- generator supplied by 'System.Random.getStdRandom'.
evalRandIO :: Rand StdGen a -> IO a
evalRandIO m = getStdRandom (runRand m)



instance (RandomGen g) => MonadRandom (Rand g) where
    getRandom = MkRand $ fst . random
    getRandoms = MkRand $ randoms
    getRandomR range = MkRand $ fst . randomR range
    getRandomRs range = MkRand $ randoms 

instance (RandomGen g) => MonadSplit g (Rand g) where
    getSplit = MkRand id  -- I think this has to be right ---NR

--------------------------------------------------------------------------
-- the end.
