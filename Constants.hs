{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Constants where

import Data.Vector.Unboxed
import Data.Ix
import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import Data.Vector.Unboxed as U hiding (minimum, (++), map)
import qualified Data.Vector as V hiding (minimum, (++), map)


type Alphabet = U.Vector Char
  
maxProb = 10e1024 :: Double
aminoList = "ACDEFGHIKLMNPQRSTVWYX"
amino = fromList aminoList
nucleotide = fromList "ACTG"
betaCoeff = 0.5

-- amino = fromList aminoS :: Alphabet 
-- nucleotide = fromList nucleotideS 

newtype AA = AA Int
  deriving (Show, Eq, Num, Ord, Ix, Integral, Enum, Real
           , Data.Vector.Generic.Base.Vector U.Vector
           , Data.Vector.Generic.Mutable.MVector U.MVector, U.Unbox)
  
instance Bounded AA where
     minBound = AA 0
     maxBound = AA 20

-- aaProb table (AA i) = table U.! i

(/!/) :: Unbox a => U.Vector a -> AA -> a

infix /!/
x /!/ (AA i) = x U.! i


getResidue :: Alphabet -> AA -> Char
getResidue alpha (AA i) = alpha ! i

numAlphabetAdditions = 1 :: Int -- just X for now

data Debugging = Debugging { slicing :: Bool }
debug = Debugging { slicing = False }

