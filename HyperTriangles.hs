{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HyperTriangles
       ( countEnumLaw, pointsWidthLaw, twoCountsLaw, freqLaw, randomLaw
       , isEnumLaw
       , pointInTri, Dimension(..), Length(..)
       , hyperProps
       )
       where

-- ^ Functions for counting and finding points in hypertriangles
import Control.Monad.LazyRandom
import Data.Ratio

import Test.QuickCheck
import Test.QuickCheck.Monadic

hyperProps :: [(String, Property)]
hyperProps = [ ("randomTriangle", property randomLaw)
             , ("countEnum", property countEnumLaw)
             , ("pointsWidth", property pointsWidthLaw)
             , ("isEnum", property isEnumLaw)
             , ("twoCounts", property twoCountsLaw)
             , ("freq", property freqLaw)
             ]


newtype Dimension = D Int
  deriving (Enum, Show)
           
newtype Length    = L Int
  deriving (Show)

-- | @pointsInTri k n@ says how many ways there 
-- are to add @k@ nonnegative integers to sum to @n@.
nPointsInTri :: Dimension -> Length -> Int
nPointsInTri (D 1) _     = 1
nPointsInTri  _     (L 0) = 1
nPointsInTri k (L n) = sum [ nPointsInTri (pred k) (L (n - d0)) | d0 <- [0..n] ]

-- | @pointsInTri k n@ enumerates all combinations
-- of @k@ nonnegative that sum to @n@.
pointsInTri :: Dimension -> Length -> [[Int]]
pointsInTri (D 1) (L n) = [[n]]
pointsInTri (D k) (L 0) = [take k $ repeat 0]
pointsInTri k (L n) =
  [ d0 : points | d0 <- [0..n], points <- pointsInTri (pred k) (L(n-d0)) ]


nPoints' (D k) (L n) = productRising (succ n) (pred k) `div` fact (pred k)
  where fact 0 = 1
        fact n = n * fact (pred n)
        productRising n 0 = 1
        productRising n k = n * productRising (succ n) (pred k)

-- | @freq k n i@ is the fraction of times @i@ occurs as the
-- first element when the k number summing to @n@ are enumerated.
freq :: Dimension -> Length -> Int -> Ratio Int
freq (D 1) (L n) i = if i == n then 1 else 0
freq k (L n) i = nPoints' (pred k) (L (n-i)) % nPoints' k (L n)

-- | @countI k n i@ is the number of times @i@ occurs as the
-- first element when the k number summing to @n@ are enumerated.
countI :: Dimension -> Length -> Int -> Int
countI (D 1) (L n) i = if i == n then 1 else 0
countI k (L n) i = nPoints' (pred k) (L (n-i))

pointInTri :: RandomGen r => Dimension -> Length -> Rand r [Int]
pointInTri (D 1) (L n) = return [n]
pointInTri (D k) (L 0) = return $ take k $ repeat 0
pointInTri k l@(L n) =
  weightedRandom [ (countI k l i, fmap (i:) (pointInTri (pred k) (L (n-i))))
                 | i <- [0..n]
                 ]

isInTri (D k) (L n) ns = length ns == k && sum ns == n && all (>= 0) ns

newtype Count = Count Int
  deriving (Show)

randomLaw :: Count -> Dimension -> Length -> Property
randomLaw (Count count) k n = monadicIO $ do
  points <- run $ evalRandIO $ sequence $ take count $ repeat $ pointInTri k n
  assert $ all (isInTri k n) points
  

unPos (Positive n) = n
unNn (NonNegative n) = n
unCount (Count n) = n

instance Arbitrary Dimension where
  arbitrary = fmap D $ sized (\n -> choose (1, max 1 (min n 7)))
  shrink (D n) = map (D. unPos) $ shrink $ Positive n

instance Arbitrary Length where
  arbitrary = fmap L $ sized (\n -> choose (0, min n 30))
  shrink (L n) = map (L . unNn) $ shrink $ NonNegative n

instance Arbitrary Count where
  arbitrary = fmap Count $ sized (\n -> choose (0, min n 10000))
  shrink (Count n) = map (Count . unPos) $ shrink $ Positive n

countEnumLaw k n = nPointsInTri k n == length (pointsInTri k n)
pointsWidthLaw d@(D k) n = all ((== k) . length) (pointsInTri d n)
                           
isEnumLaw k n = all (isInTri k n) (pointsInTri k n)


twoCountsLaw k n = nPointsInTri k n == nPoints' k n 

freqLaw k (L n) = all goodI [0..n]
  where goodI i = freq k (L n) i == length match % length allPoints
          where match = filter ((== i) . head) allPoints
                allPoints = pointsInTri k (L n)
                
                
weightedRandom :: RandomGen r => [(Int, Rand r a)] -> Rand r a
weightedRandom [] = error "weightedRandom used with empty list"
weightedRandom xs = getRandomR (1, total) >>= (`pick` xs)
 where total = sum (map fst xs)
       pick n ((k,x):xs)
         | n <= k    = x
         | otherwise = pick (n-k) xs
       pick _ _  = error "bad bounds calculation in weightedRandom"

