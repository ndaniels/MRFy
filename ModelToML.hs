module ModelToML where

import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Text.Printf (printf)

import Model3
import MRFTypes (EProbs)
import Score (Score, unScore)

indent = "    "
indent2 = "      "

toml :: HMM -> String
toml (HMM hmm) =
  intercalate "\n"
  $ ["val input_hmms = [\n  Array.fromList ["]
    ++
    map (\s -> indent ++ s) (intersperse ", " $ V.toList $ fmap ntoml hmm)
    ++
    ["  ]\n]"]
  where us :: Score -> String
        us s = let d = unScore s
               in  if isInfinite d then "min_prob" else printf "%f" d

        ntoml :: Node -> String
        ntoml n =
          printf ("NODE {\n" ++
                  "%sm_i = %s, m_m = %s, m_d = %s,\n" ++
                  "%si_i = %s, i_m = %s,\n%sd_m = %s, d_d = %s,\n" ++
                  "%sm_emission = Array.fromList [%s],\n" ++
                  "%si_emission = Array.fromList [%s]\n%s}")
            indent2
            (us $ m_i $ m n) (us $ m_m $ m n) (us $ m_d $ m n)
            indent2
            (us $ i_i $ i n) (us $ i_m $ i n)
            indent2
            (us $ d_m $ d n) (us $ d_d $ d n)
            indent2 (etoml memit) indent2 (etoml iemit) indent
          where memit = m_emission $ m n
                iemit = i_emission $ i n

        etoml :: EProbs -> String
        etoml = intercalate ", " . map us . U.toList

