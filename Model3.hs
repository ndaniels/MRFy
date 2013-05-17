{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}



{- See note and diagram at 
  https://www.evernote.com/shard/s276/sh/39e47600-3354-4e8e-89f8-5c89884f9245/8880bd2c2a94dffb9be1432f12471ff2
-}


module Model3
where

--import Text.Printf (printf)
--import Test.QuickCheck
--import Test.QuickCheck.Arbitrary
--import Test.QuickCheck.Gen

import qualified Data.Array as A
--import qualified Constants as C
--import qualified HMMArby as Arby
import qualified MRFTypes as T
import qualified Score as S

data MState = M { m_i, m_m, m_d :: S.Score
                , m_emission :: T.EProbs
                }
  deriving (Eq, Show)

data IState = I { i_i, i_m :: S.Score
                , i_emission :: T.EProbs
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
