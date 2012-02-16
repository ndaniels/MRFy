module NonUniform(randomsDist) where

import Data.Array
import Data.List
import System.Random

genTable :: (Num a, Ord a) => [a] -> (Array Int a, Array Int Int)
genTable ps =
    let n = length ps
        n' = fromIntegral n
        (small, large) = partition ((< 1) . snd) $ zip [0..] $ map (n' *) ps
        loop ((l, pl):ls) ((g, pg):gs) probs aliases =
            let prob = (l,pl)
                alias = (l,g)
                pg' = (pg + pl) - 1
                gpg = (g, pg')
            in  if pg' < 1 then loop (gpg:ls) gs (prob:probs) (alias:aliases)
                          else loop ls (gpg:gs) (prob:probs) (alias:aliases)
        loop ls gs probs aliases = loop' (ls ++ gs) probs aliases
        loop' [] probs aliases = (array (0,n-1) probs, array (0,n-1) aliases)
        loop' ((g,_):gs) probs aliases = loop' gs ((g,1):probs) ((g, -1):aliases)
    in  loop small large [] []

-- | Generate an infinite list of random values with the given distribution.
-- The probabilities are scaled so they do not have to add up to one.
-- 
-- Uses Vose's alias method for generating the values.
-- For /n/ values this has O(/n/) setup complexity and O(1) complexity for each
-- generated item.
randomsDist :: (RandomGen g, Random r, Fractional r, Ord r)
            => g                           -- | random number generator
            -> [(a, r)]                    -- | list of values with the probabilities
            -> [a]
randomsDist g xps =
    let (xs, ps) = unzip xps
        n = length xps
        axs = listArray (0, n-1) xs
        s = sum ps
        (probs, aliases) = genTable $ map (/ s) ps
        (g', g'') = split g
        is = randomRs (0, n-1) g'
        rs = randoms g''
        ks = zipWith (\ i r -> if r <= probs!i then i else aliases!i) is rs
    in  map (axs!) ks
