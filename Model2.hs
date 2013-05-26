{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Model2
where

import Text.Printf (printf)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import qualified Data.Array as A
import qualified Constants as C
import qualified HMMArby as Arby
import qualified MRFTypes as T
import qualified Score as S

data MState = M { m_i, m_m, m_d :: Score
                , m_emission :: T.EProbs
                }
  deriving (Eq, Show)

data IState = I { i_i, i_m :: Score
                , i_emission :: T.EProbs
                }
  deriving (Eq, Show)
              
data DState = D { d_m, d_d :: Score }
  deriving (Eq, Show)

data BState = B { b_i, b_m, b_d :: Score }
  deriving (Eq, Show)

data EState = E
  deriving (Eq, Show)

data MiddleNode = Middle { m :: MState, d :: DState, i :: IState }
  deriving (Eq, Show)
data BeginNode  = Begin  { b :: BState, bi :: IState }
  deriving (Eq, Show)
data EndNode    = End    { e :: EState }
  deriving (Eq, Show)

data Sequence i a = Sequence { count :: Int, get :: i -> a }

data Model = Model { begin  :: BeginNode
                   , middle :: Sequence NodeIndex MiddleNode
                   , end    :: EndNode
                   }

asBeginState :: MState -> BState
asBeginState s = B { b_i = m_i s, b_m = m_m s, b_d = m_d s }

asBegin :: MiddleNode -> BeginNode
asBegin n = Begin { b  = asBeginState (m n)
                  , bi = i n
                  }

newtype NodeIndex = NI Int deriving (Enum, Eq, A.Ix, Ord)

instance Eq Model where
  Model b1 m1 e1 == Model b2 m2 e2 =
    b1 == b2 && e1 == e2 && count m1 == count m2 &&
    and [get m1 i == get m2 i | i <- map NI [0..pred (count m1)]]


type Score = S.Score
