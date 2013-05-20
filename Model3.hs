{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}



{- See note and diagram at 
  https://www.evernote.com/shard/s276/sh/39e47600-3354-4e8e-89f8-5c89884f9245/8880bd2c2a94dffb9be1432f12471ff2
-}


module Model3
  ( Model, Sequence(..), Node(..), Slice(..)
  , NodeIndex(..)
  , MState(..), IState(..), DState(..)
  , HMM(..)
                            
  , toHMM, slice, numNodes
  )
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

data Node = Node { m :: MState, d :: DState, i :: IState }
  deriving (Eq, Show)

data Sequence i a = Sequence { count :: Int, get :: i -> a }

type Model = Sequence NodeIndex Node

newtype NodeIndex = NI Int deriving (Enum, Eq, A.Ix, Ord)

instance (Enum i, Eq a) => Eq (Sequence i a) where
  m1 == m2 = count m1 == count m2 &&
    and [get m1 i == get m2 i | i <- [toEnum 0..pred (toEnum (count m1))]]

newtype HMM = HMM (V.Vector Node)
  deriving (Eq)

toHMM :: T.HMM -> HMM
toHMM old_nodes = HMM (fmap toNode old_nodes)
  where toNode n = Node m d i
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

data Slice = Slice { width :: Int, nodes_skipped :: Int }

slice :: HMM -> Slice -> Model
slice (HMM nodes) s = Sequence (width s) mid
  where mid :: NodeIndex -> Node
        mid (NI n)
          | n < 0         = error ("< oob (" ++ show n ++ ")")
          | n > (width s) = error "> oob"
          | otherwise     = nodes V.! (n + nodes_skipped s)

numNodes :: HMM -> Int
numNodes (HMM nodes) = V.length nodes
