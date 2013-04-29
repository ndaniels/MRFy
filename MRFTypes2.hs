module MRFTypes2
where

import qualified Data.Array as A

import qualified MRFTypes as T
import Score (negLogZero)

type HMM = A.Array Int Node

data Node = Begin BeginNode
            | Middle MiddleNode
            | End EndNode 

data BeginNode = BeginNode { bmate :: T.EProbs
                           , binse :: T.EProbs
                           , b_m :: T.TProb
                           , b_i :: T.TProb
                           }
data EndNode = EndNode { emate :: T.EProbs
                       , m_e :: T.TProb
                       }

data MiddleNode = MiddleNode { mate :: T.EProbs
                             , inse :: T.EProbs
                             , m_m :: T.TProb
                             , m_i :: T.TProb
                             , m_d :: T.TProb
                             , i_m :: T.TProb
                             , i_i :: T.TProb
                             , d_m :: T.TProb
                             , d_d :: T.TProb
                             }

asBegin :: Node -> BeginNode
asBegin (End _) = error "wat? (asBegin)"
asBegin (Begin n) = n
asBegin (Middle n) = BeginNode (mate n) (inse n) (m_m n) (m_i n)

asEnd :: Node -> EndNode
asEnd (End n) = n
asEnd (Begin _) = error "wat? (asEnd)"
asEnd (Middle n) = EndNode (mate n) (T.TProb negLogZero)

asMiddle :: Node -> MiddleNode
asMiddle (Middle n) = n
asMiddle _ = error "wat? (asMiddle)"

data Model = Model { begin :: BeginNode
                   , middle :: Int -> MiddleNode
                   , end :: EndNode
                   }

type Interval = (Int, Int)

slice :: HMM -> Interval -> Model
slice hmm (start, len) =
  Model (asBegin $ hmm A.! start) mid (asEnd $ hmm A.! (start + len - 1))
  where mid :: Int -> MiddleNode
        mid n
          | n < 0     = error "< oob"
          | n >= len  = error "> oob"
          | otherwise = asMiddle $ hmm A.! (start + n)

