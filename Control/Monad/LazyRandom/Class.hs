{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies #-}

module Control.Monad.LazyRandom.Class (
    MonadRandom,
    getRandom,
    getRandomR,
    getRandoms,
    getRandomRs,
    MonadSplit,
    getSplit
    ) where

import System.Random

-- | An interface to random number generation monads.
class (Monad m) => MonadRandom m where
    -- | Return a randomly-selected value of type @a@.  See
    -- 'System.Random.random' for details.
    getRandom :: (Random a) => m a
    -- | Return an infinite stream of random values of type @a@.  See
    -- 'System.Random.randoms' for details.
    getRandoms :: (Random a) => m [a]
    -- | Return a randomly-selected value of type @a@ in the range
    -- /(lo,hi)/.  See 'System.Random.randomR' for details.
    getRandomR :: (Random a) => (a,a) -> m a
    -- | Return an infinite stream of randomly-selected value of type @a@
    -- in the range /(lo,hi)/.  See 'System.Random.randomRs' for details.
    getRandomRs :: (Random a) => (a,a) -> m [a]

-- | An interface to monads with splittable state (as most random number generation monads will have).
-- The intention is that the 'getSplit' action splits the state, returning one half of the result, and
-- setting the new state to the other.
class (Monad m) => MonadSplit s m | m -> s where
    getSplit :: m s

