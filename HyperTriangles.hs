{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HyperTriangles
       ( countEnumLaw, pointsWidthLaw
       )
       where

-- ^ Functions for counting and finding points in hypertriangles

import Test.QuickCheck


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

unPos (Positive n) = n
unNn (NonNegative n) = n

instance Arbitrary Dimension where
  arbitrary = fmap D $ sized (\n -> choose (1, min n 7))
  shrink (D n) = map (D. unPos) $ shrink $ Positive n

instance Arbitrary Length where
  arbitrary = fmap L $ sized (\n -> choose (0, min n 30))
  shrink (L n) = map (L . unNn) $ shrink $ NonNegative n

countEnumLaw k n = nPointsInTri k n == length (pointsInTri k n)
pointsWidthLaw d@(D k) n = all ((== k) . length) (pointsInTri d n)
                           

