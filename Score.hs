{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving #-}

module Score
       ( Score(..), negLogZero, negLogOne, unScore
       , Scored(..), (/+/)
       )
where

import Control.Monad (liftM)
import Data.Function
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U

-- @ start score.tex
newtype Score = Score Double
-- @ end score.tex
  deriving (Eq, Ord)
  -- deriving (Eq, Ord, Vector U.Vector, MVector U.MVector, U.Unbox) 
 -- ^ A "score" is the negated logarithm of a probability
                
negLogZero :: Score -- ^ Stands in for - log 0
negLogZero = Score 10e1024

negLogOne :: Score -- ^ - log 1
negLogOne = Score 0.0

unScore :: Score -> Double
unScore (Score x) = x

instance Show Score where
  show (Score x) = show x

instance Num Score where
  Score x + Score y = Score $ x + y
  Score x - Score y = Score $ x - y
  Score _ * Score _ = error "multiplying log probabilities makes no sense"
  negate (Score _) = error "negating a Score is not permitted"
  abs (Score _) = error "absolute value of Score is senseless"
  signum (Score _) = error "signum of Score is senseless"
  fromInteger = Score . fromInteger

-- @ start vscore.tex
data Scored a = Scored { unScored :: !a, scoreOf :: {-# UNPACK #-} !Score}
(/+/) :: Score -> Scored a -> Scored a
-- @ end vscore.tex
infix /+/
x /+/ Scored a y = Scored a (x + y)

instance Eq (Scored a) where
  (==) = (==) `on` scoreOf
instance Ord (Scored a) where
  (<) =  (<) `on` scoreOf
  compare = compare `on` scoreOf
instance Show a => Show (Scored a) where
  show s = "(Score: " ++ (show $ scoreOf s) ++ ", " ++ (show $ unScored s) ++ ")"

instance Functor Scored where
  fmap f (Scored a x) = Scored (f a) x

newtype instance U.MVector s (Score) = MV_Score (U.MVector s Double)
newtype instance U.Vector    (Score) = V_Score  (U.Vector    Double)

instance U.Unbox Score

instance M.MVector U.MVector Score where
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
  basicLength (MV_Score v) = M.basicLength v
  basicUnsafeSlice i n (MV_Score v) = MV_Score $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Score v1) (MV_Score v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Score `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Score x) = MV_Score `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Score v) i = Score `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Score v) i (Score x) = M.basicUnsafeWrite v i x
  basicClear (MV_Score v) = M.basicClear v
  basicSet (MV_Score v) (Score x) = M.basicSet v x
  basicUnsafeCopy (MV_Score v1) (MV_Score v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Score v1) (MV_Score v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Score v) n = MV_Score `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Score where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Score v) = V_Score `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Score v) = MV_Score `liftM` G.basicUnsafeThaw v
  basicLength (V_Score v) = G.basicLength v
  basicUnsafeSlice i n (V_Score v) = V_Score $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Score v) i = Score `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Score mv) (V_Score v) = G.basicUnsafeCopy mv v
  elemseq _ (Score x) y = G.elemseq (undefined :: U.Vector a) x y

