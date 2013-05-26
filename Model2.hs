{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Model2
where

--import Text.Printf (printf)
--import Test.QuickCheck
--import Test.QuickCheck.Arbitrary
--import Test.QuickCheck.Gen

import qualified Data.Array as A
import qualified Data.Vector as V
--import qualified Constants as C
--import qualified HMMArby as Arby
import qualified MRFTypes as T
import qualified Score as S

data MState = M { m_i, m_m, m_d :: S.Score
                , m_emission :: T.EScores
                }
  deriving (Eq, Show)

data IState = I { i_i, i_m :: S.Score
                , i_emission :: T.EScores
                }
  deriving (Eq, Show)
              
data DState = D { d_m, d_d :: S.Score }
  deriving (Eq, Show)

data BState = B { b_i, b_m, b_d :: S.Score }
  deriving (Eq, Show)

data MiddleNode = Middle { m :: MState, d :: DState, i :: IState }
  deriving (Eq, Show)
data BeginNode  = Begin  { b :: BState, bi :: IState }
  deriving (Eq, Show)

data Sequence i a = Sequence { count :: Int, get :: i -> a }

type Node = MiddleNode

data Model = Model { begin  :: BeginNode
                   , middle :: Sequence NodeIndex MiddleNode
                   }

class State s where
  toM :: s -> S.Score
  toI :: s -> S.Score
  toD :: s -> S.Score

instance State MState where
  toM = m_m
  toI = m_i
  toD = m_d
  
instance State DState where
  toM = d_m
  toI = error "d-i transition"
  toD = d_d
  
instance State IState where
  toM = i_m
  toI = i_i
  toD = error "i-d transition"
  
instance State BState where
  toM = b_m
  toI = b_i
  toD = b_d
  


asBeginState :: MState -> BState
asBeginState s = B { b_i = m_i s, b_m = m_m s, b_d = m_d s }

asBegin :: MiddleNode -> BeginNode
asBegin n = Begin { b  = asBeginState (m n)
                  , bi = i n
                  }

newtype NodeIndex = NI Int deriving (Enum, Eq, A.Ix, Ord)

instance Eq Model where
  Model b1 m1 == Model b2 m2 =
    b1 == b2 && count m1 == count m2 &&
    and [get m1 i == get m2 i | i <- map NI [0..pred (count m1)]]



data Slice = Slice { width :: Int, nodes_skipped :: Int }

toHMM :: a -> a
toHMM = id

slice :: T.HMM -> Slice -> Model
slice onodes s = Model { begin = asBegin (nodes V.! nodes_skipped s)
                              , middle = Sequence (width s - 1) mid }
  where mid :: NodeIndex -> Node
        mid (NI n)
          | n < 0             = error ("< oob (" ++ show n ++ ")")
          | n > (width s - 1) = error "> oob"
          | otherwise         = nodes V.! (n + (nodes_skipped s + 1))
        nodes = fmap toNode onodes
        toNode n = Middle m d i
          where m = M { m_i = T.m_i trans
                      , m_m = T.m_m trans
                      , m_d = T.m_d trans
                      , m_emission = T.matEmissions n
                      }
                i = I { i_i = T.i_i trans
                      , i_m = T.i_m trans
                      , i_emission = T.insEmissions n
                      }
                d = D { d_d = T.d_d trans
                      , d_m = T.d_m trans
                      }
                trans = T.transitions n

numNodes :: T.HMM -> Int
numNodes nodes = V.length nodes
