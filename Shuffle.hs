module Shuffle
       ( shuffle, shuffle' )
where

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.LazyRandom
import Control.Monad.ST
import Data.STRef
 
-- Taken from:  http://www.haskell.org/haskellwiki/Random_shuffle
-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
-- cf Data.Random.Extras.shuffle at O(N log N)
shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle gen xs = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

shuffle' :: RandomGen r => [a] -> Rand r [a]
shuffle' xs =
  do swaps <- forM [1..n] $ \i -> do { j <- getRandomR (i, n); return (i, j) }
     return $ runST $
         do ar <- newArray n xs
            mapM (swap ar) swaps
  where n = length xs
        newArray :: Int -> [a] -> ST s (STArray s Int a)
        newArray n xs =  newListArray (1,n) xs

        swap ar (i, j) = do vi <- readArray ar i
                            vj <- readArray ar j
                            writeArray ar j vi
                            return vj
