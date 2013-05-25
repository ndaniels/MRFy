module ModelToC where

import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Text.Printf (printf)

import Model3
import MRFTypes (EProbs)
import Score (Score, unScore)

indent = "    "

toc :: HMM -> String
toc (HMM hmm) =
  intercalate "\n"
  $ ["struct HMM *input_hmm = &(struct HMM){" ++ show hmmlen ++ ", {"]
    ++
    map (\s -> indent ++ s)
      (intersperse ", " $ V.toList $ fmap ntoc hmm)
    ++
    ["}};"]
  where hmmlen = V.length hmm

        us :: Score -> String
        us s = let d = unScore s
               in  if isInfinite d then "DBL_MAX" else printf "%f" d

        ntoc :: Node -> String
        ntoc n =
          printf "{ %s, %s, %s, %s, %s, %s, %s,\n%s{ %s },\n%s{ %s }\n%s}"
            (us $ m_i $ m n) (us $ m_m $ m n) (us $ m_d $ m n)
            (us $ i_i $ i n) (us $ i_m $ i n)
            (us $ d_m $ d n) (us $ d_d $ d n)
            indent (etoc memit) indent (etoc iemit) indent
          where memit = m_emission $ m n
                iemit = i_emission $ i n

        etoc :: EProbs -> String
        etoc = intercalate ", " . map us . U.toList

