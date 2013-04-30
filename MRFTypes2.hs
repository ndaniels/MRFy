module MRFTypes2
where

import Debug.Trace

import qualified Data.Array as A
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import qualified Constants as C
import qualified HMMArby as Arby
import qualified MRFTypes as T
import qualified Score as S

newtype HMM = HMM (BeginNode, A.Array Int MiddleNode, EndNode)
              deriving (Show, Eq)

numNodes :: HMM -> Int
numNodes (HMM (hmmBegin, mids, hmmEnd)) = 2 + (end - start + 1)
  where (start, end) = A.bounds (mids)

data BeginNode = BeginNode { bmate :: T.EProbs
                           , binse :: T.EProbs
                           , b_m_m :: T.TProb
                           , b_m_i :: T.TProb
                           , b_m_d :: T.TProb
                           , b_i_m :: T.TProb
                           , b_i_i :: T.TProb
                           }
                 deriving (Show, Eq)


-- We are choosing to represent the last "real" node as an end
-- node, even though the Viterbi recurrence starts on a kind-of
-- virtual node proceeding it.
-- Alternatively, we could have an end node that contains no
-- members and modify the previous nodes transition probabilities.
-- Or, a 4-tuple.
data EndNode = EndNode { emate :: T.EProbs
                       , einse :: T.EProbs
                       , e_m_m :: T.TProb
                       , e_m_i :: T.TProb
                       , e_i_m :: T.TProb
                       , e_i_i :: T.TProb
                       , e_d_m :: T.TProb
                       }
               deriving (Show, Eq)

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
                 deriving (Show, Eq)

asBegin :: MiddleNode -> BeginNode
asBegin n = BeginNode (mate n) (inse n) (m_m n) (m_i n) (m_d n) (i_m n) (i_i n)

asEnd :: MiddleNode -> EndNode
asEnd n = EndNode (mate n) (inse n) (m_m n) (m_i n) (i_m n) (i_i n) (d_m n)

data Model = Model { begin :: BeginNode
                   , middle :: Int -> MiddleNode
                   , end :: EndNode
                   , size :: Int
                   }

instance Eq Model where
  m1 == m2 = modelList m1 == modelList m2

instance Show Model where
  show m = "(MODEL: size = " ++ show (size m) ++ ") "

type Interval = (Int, Int)

-- This is bunk. Not every `Model` is a valid `HMM`. Namely, a `Model`
-- can have a single node (just the `BeginNode`), while an `HMM` needs
-- at least a `BeginNode` and an `EndNode`.
modelHMM :: Model -> HMM
modelHMM = listHMM . modelList
  where listHMM (Just beg, mids, Just end) = HMM (beg, listArray mids, end)

modelList :: Model -> (Maybe BeginNode, [MiddleNode], Maybe EndNode)
modelList = mfoldr fbeg fmid fend (Nothing, [], Nothing)
  where fbeg n (_, mids, end) = (Just n, mids, end)
        fmid n (beg, mids, end) = (beg, n:mids, end)
        fend n (beg, mids, _) = (beg, mids, Just n)

mfoldr
  :: (BeginNode -> a -> a)
  -> (MiddleNode -> a -> a)
  -> (EndNode -> a -> a)
  -> a
  -> Model
  -> a
mfoldr bf mf ef init mod
  | size mod == 0 = init
  | size mod == 1 = bf (begin mod) init
  | otherwise = bf (begin mod) (mfold' 0)
    where mfold' n
            | n == size mod - 2 = ef (end mod)       init
            | otherwise         = mf (middle mod n) (mfold' (n + 1))

slice :: HMM -> Interval -> Model
slice hmm@(HMM (hmmBegin, mids, hmmEnd)) (start, len) =
  Model begNode mid endNode len
  where mid :: Int -> MiddleNode
        mid n
          | n < 0         = error "< oob"
          | n >= len - 2  = error "> oob"
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
        endNode = if   start + len == numNodes hmm 
                  then hmmEnd
                  else asEnd (mids A.! (start + len - 2))

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
  shrink (HMM (beg, mids, end)) = [HMM (beg, m, end) | m <- mids' ]
    where mids' = map listArray $ shrink $ A.elems mids
  arbitrary = do
    (beg, mids, end) <- arbitrary :: Gen (BeginNode, [MiddleNode], EndNode)
    return $ HMM (beg, listArray mids, end)

instance Arbitrary BeginNode where
  arbitrary = do
    mate <- randEProbs
    inse <- randEProbs
    (bm, bi) <- arbitrary :: Gen (T.TProb, T.TProb)
    return $ BeginNode mate inse bm bi

instance Arbitrary EndNode where
  arbitrary = do
    mate <- randEProbs
    me <- arbitrary :: Gen T.TProb
    return $ EndNode mate me

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
  len == count
  where count = mfoldr inc inc inc 0 mod
        mod = slice hmm (start, len)
        inc _ n = n + 1

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
propSliceLenFull hmm = numNodes hmm == size (slice hmm (0, numNodes hmm))

