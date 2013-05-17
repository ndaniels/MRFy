{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model
where

import Debug.Trace

import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import qualified Constants as C
import qualified HMMArby as Arby
import qualified MRFTypes as T
import qualified Score as S

newtype HMM = HMM (BeginNode, A.Array Int MiddleNode)
              deriving (Eq)

instance Show HMM where
  show (HMM (beg, mids)) = printf "HMM\n%s\n%s\n"
                                  (show beg) (show $ A.elems mids)

numNodes :: HMM -> Int
numNodes (HMM (_, mids)) = 1 + (end - start + 1)
  where (start, end) = A.bounds (mids)

data BeginNode = BeginNode { -- three transitions out of the Begin state
                             b_m :: T.TProb
                           , b_i :: T.TProb
                           , b_d :: T.TProb
                             -- self-edge for the Insert state
                           , binse :: T.EProbs
                           , b_i_i :: T.TProb
                             -- one transition out of Insert state
                           , b_i_m :: T.TProb
                           }
                 deriving (Eq)

instance Show BeginNode where
  show b = printf "BEGIN (inse %f, trans %f)"
                  (S.unScore $ U.sum $ binse b) (transSum)
    where transSum = sum $ map (S.unScore . T.logProbability) probs
          probs = [b_m b, b_i b, b_d b, b_i_m b, b_i_i b]


-- We are choosing to represent the last "real" node as an end
-- node, even though the Viterbi recurrence starts on a kind-of
-- virtual node proceeding it.
-- Alternatively, we could have an end node that contains no
-- members and modify the previous nodes transition probabilities.
-- Or, a 4-tuple
--
--
--
data EndNode = EndNode -- always in match state; no transitions out
               deriving (Eq, Show)

data MiddleNode = MiddleNode { mate :: T.EProbs
                             , inse :: T.EProbs -- goes with i_i edge
                               -- m_i and i_i are edges within the node
                               -- other edges are to the next node
                             , m_m :: T.TProb
                             , m_i :: T.TProb
                             , m_d :: T.TProb
                             , i_m :: T.TProb
                             , i_i :: T.TProb
                             , d_m :: T.TProb
                             , d_d :: T.TProb
                             }
                 deriving (Eq)

instance Show MiddleNode where
  show n = printf "MIDDLE (mate %f, inse %f, trans %f)"
                  (S.unScore $ U.sum $ mate n)
                  (S.unScore $ U.sum $ inse n)
                  (transSum)
    where transSum = sum $ map (S.unScore . T.logProbability) probs
          probs = [m_m n, m_i n, m_d n, i_m n, i_i n, d_m n, d_d n]

asBegin :: MiddleNode -> BeginNode
asBegin n = BeginNode { binse = inse n
                      , b_m = m_m n
                      , b_i = m_i n
                      , b_d = m_d n
                      , b_i_i = i_i n
                      , b_i_m = i_m n
                      }

-- data Seq a = Seq { get :: Int -> a, seqLength :: Int }  

newtype NodeIndex = NI Int deriving (Enum, Eq, A.Ix, Ord)

data Model = Model { begin :: BeginNode
                   , middle :: NodeIndex -> MiddleNode
                       -- defined on [0..pred (NI midSize)]
                   , end :: EndNode
                   , midSize :: Int
                   }

instance Eq Model where
  m1 == m2 = modelList m1 == modelList m2

instance Show Model where
  show m = "(MODEL: size = " ++ show (midSize m) ++ ") "

type Interval = (Int, Int)

-- This is bunk. Not every `Model` is a valid `HMM`. Namely, a `Model`
-- can have a single node (just the `BeginNode`), while an `HMM` needs
-- at least a `BeginNode` and an `EndNode`.
modelHMM :: Model -> HMM
modelHMM mod = HMM (beg, listArray mids)
  where (beg, mids, _) = modelList mod

modelList :: Model -> (BeginNode, [MiddleNode], EndNode)
modelList mod = (begin mod, reverse $ midsList (midSize mod - 1), end mod)
  where midsList (-1) = []
        midsList   n  = mid (NI n) : midsList (n - 1)

        mid = middle mod

slice :: HMM -> Interval -> Model
slice hmm@(HMM (hmmBegin, mids)) (start, len) =
  Model begNode mid EndNode (len - 1)
  where mid :: NodeIndex -> MiddleNode
        mid (NI n)
          | n < 0         = error ("< oob (" ++ show n ++ ")")
          | n >= len - 1  = error "> oob"
          -- Gross. Indexing here must be offset by whether the
          -- begin node was the real begin node or if it was
          -- plucked from the middle nodes.
          | otherwise     = mids A.! (n + max 0 (start - 1))

        -- If the slice bumps up against the begin/end of the HMM,
        -- then use the real begin/end nodes. Otherwise convert
        -- middle nodes to begin/end.
        begNode = if   start == 0
                  then hmmBegin
                  else asBegin (mids A.! (start - 1))

listArray :: [a] -> A.Array Int a
listArray xs = A.listArray (0, length xs - 1) xs



--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

randEProbs :: Gen T.EProbs
randEProbs = do
    eprobs <- sequence $ take (length C.aminoList) $ repeat prob
    return $ U.fromList $ map toScore eprobs
    where prob :: Gen Double
          prob = choose (0.0, 1.0)

          toScore :: Double -> S.Score
          toScore f = if f == 0.0 then S.negLogZero else S.Score (- log f)

instance Arbitrary HMM where
  shrink (HMM (beg, mids)) = [HMM (beg, m) | m <- mids' ]
    where mids' = map listArray $ shrink $ A.elems mids
  arbitrary = do
    (beg, mids, end) <- arbitrary :: Gen (BeginNode, [MiddleNode], EndNode)
    return $ HMM (beg, listArray mids)

instance Arbitrary BeginNode where
  arbitrary = do
    inse <- randEProbs
    (mm, mi, md) <- arbitrary :: Gen (T.TProb, T.TProb, T.TProb)
    (im, ii) <- arbitrary :: Gen (T.TProb, T.TProb)
    return $ BeginNode inse mm mi md im ii

instance Arbitrary EndNode where
  arbitrary = return EndNode

instance Arbitrary MiddleNode where
  arbitrary = do
    mate <- randEProbs
    inse <- randEProbs
    (mm, mi, md) <- arbitrary :: Gen (T.TProb, T.TProb, T.TProb)
    (im, ii, dm, dd) <- arbitrary :: Gen (T.TProb, T.TProb, T.TProb, T.TProb)
    return $ MiddleNode mate inse mm mi md im ii dm dd

instance Arbitrary Model where
  shrink mod = map (\hmm -> slice hmm (0, numNodes hmm)) $ shrink $ modelHMM mod
  arbitrary = do
    hmm <- arbitrary :: Gen HMM
    return $ slice hmm (0, (numNodes hmm))


--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

newtype Sliceable = Sliceable (Interval, HMM)
                    deriving (Show)

instance Arbitrary Sliceable where
  -- shrink ??? seems difficult/tedious
  arbitrary = do
    hmm <- arbitrary :: Gen HMM
    start <- choose (0, numNodes hmm - 2) :: Gen Int
    len <- choose (0, numNodes hmm - start) :: Gen Int
    return $ Sliceable ((start, len), hmm)

-- len == |slice (start, len) hmm|
propSliceLen :: Sliceable -> Bool
propSliceLen (Sliceable ((start, len), hmm)) =
  len == 1 + midSize mod
  where mod = slice hmm (start, len)

-- slice hmm = (slice . toHMM . slice) hmm
propInvertible :: Sliceable -> Bool
propInvertible (Sliceable ((start, len), hmm)) =
  if len <= 2
  then True
  else sliced == slice slicedHMM (0, numNodes slicedHMM)
  where sliced = slice hmm (start, len)
        slicedHMM = modelHMM sliced

-- hmm == modelHMM . slice (0, |hmm|) hmm
propInverse :: HMM -> Bool
propInverse hmm = hmm == hmm'
  where hmm' = modelHMM $ slice hmm (0, numNodes hmm)

-- |hmm| == |slice (0, |hmm|) hmm|
-- subsumed by `propSliceLen` i think
propSliceLenFull :: HMM -> Bool
propSliceLenFull hmm = numNodes hmm == 1 + midSize (slice hmm (0, numNodes hmm))

traceid :: (Show a) => a -> a
traceid x = trace ("[[TRACE: " ++ show x ++ "]]") x


--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

toHMM :: T.HMM -> HMM
toHMM ohmm = HMM (asBegin $ toMidNode obnode, listArray $ map toMidNode omids)
  where obnode = V.head ohmm
        omids  = V.toList $ V.drop 1 ohmm

toMidNode :: T.HMMNode -> MiddleNode
toMidNode on =
  MiddleNode { mate = T.matEmissions on
             , inse = T.insEmissions on
             , m_m  = T.m_m trans
             , m_i  = T.m_i trans
             , m_d  = T.m_d trans
             , i_m  = T.i_m trans
             , i_i  = T.i_i trans
             , d_m  = T.d_m trans
             , d_d  = T.d_d trans
             }
  where trans = T.transitions on

