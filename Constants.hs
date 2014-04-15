{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, BangPatterns, GeneralizedNewtypeDeriving #-}

module Constants where

import Control.Monad (liftM)
import Data.Vector.Unboxed
import Data.Ix
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
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

-- @ start aa.tex
newtype AA = AA Int
-- @ end aa.tex
  deriving (Show, Eq, Num, Ord, Ix, Integral, Enum, Real)
  
instance Bounded AA where
     minBound = AA 0
     maxBound = AA ((U.length amino) - 1)
     -- maxBound = AA 2 

-- aaProb table (AA i) = table U.! i

(/!/) :: Unbox a => U.Vector a -> AA -> a

infix /!/
x /!/ (AA i) = x U.! i


getResidue :: Alphabet -> AA -> Char
getResidue alpha (AA i) = alpha ! i

numAlphabetAdditions = 1 :: Int -- just X for now

data Debugging = Debugging { slicing :: Bool }
debug = Debugging { slicing = False }


newtype instance U.MVector s (AA) = MV_AA (U.MVector s Int)
newtype instance U.Vector    (AA) = V_AA  (U.Vector    Int)

instance U.Unbox AA

instance M.MVector U.MVector AA where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_AA v) = M.basicLength v
  basicUnsafeSlice i n (MV_AA v) = MV_AA $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_AA v1) (MV_AA v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_AA `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (AA x) = MV_AA `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_AA v) i = AA `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_AA v) i (AA x) = M.basicUnsafeWrite v i x
  basicClear (MV_AA v) = M.basicClear v
  basicSet (MV_AA v) (AA x) = M.basicSet v x
  basicUnsafeCopy (MV_AA v1) (MV_AA v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_AA v1) (MV_AA v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_AA v) n = MV_AA `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector AA where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_AA v) = V_AA `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_AA v) = MV_AA `liftM` G.basicUnsafeThaw v
  basicLength (V_AA v) = G.basicLength v
  basicUnsafeSlice i n (V_AA v) = V_AA $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_AA v) i = AA `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_AA mv) (V_AA v) = G.basicUnsafeCopy mv v
  elemseq _ (AA x) y = G.elemseq (undefined :: U.Vector a) x y

