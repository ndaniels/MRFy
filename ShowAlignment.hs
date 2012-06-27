module ShowAlignment where

import Data.Char
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Beta
import Constants
import HMMPlus
import MRFTypes
import Score
import Viterbi

import Debug.Trace (trace)

-- Example output:
-- 
-- ttsmydgty-------
--         |       
-- --------LGPAEWLG 
--
-- The top sequence is the "model" or HMM sequence. All amino acids in the
-- top sequence are found by finding the most probable emission for the MATCH
-- state of that node. (Note that log probabilities are used, so we really need
-- to find the minimum.)
--
-- The bottom sequence is the query sequence (the input to smurf).
--
-- The middle is filled with spaces or pipe/bar characters depending upon state.
--
-- There are three kinds of states that affect output:
--   match:    HMM seq gets model amino acid. 
--             Middle gets pipe symbol.
--             Query seq gets query amino acid.
--   insert:   HMM seq gets '-' symbol.
--             Middle gets space character.
--             Query seq gets query amino acid.
--   deletion: HMM seq gets model amino acid.
--             Middle gets space character.
--             Query seq gets '-' symbol.

showAlignment :: HMM
              -> [BetaStrand]
              -> QuerySequence
              -> StatePath
              -> Int
              -> Alphabet
              -> String
showAlignment hmm betas query path len alpha = 
  niceify $ showA (map (getResidue alpha) $ U.toList query) path 1 Mat [] [] []
  where model i = alpha U.! ai
          where (_, ai, _) = U.foldl minWithInd 
                                     (0, 0, negLogZero) 
                                     (U.slice 0 ((U.length mEmissions) - 1) mEmissions)
                mEmissions = matchEmissions $ hmm V.! i

        minWithInd :: (Int, Int, Score) -> Score -> (Int, Int, Score)
        minWithInd (ind, mi, mp) prob = if prob < mp then
                                          (ind + 1, ind, prob)
                                        else
                                          (ind + 1, mi, mp)
  
        showA :: String -> StatePath -> Int -> StateLabel 
                 -> String -> String -> String -- correspond to the three lines
                 -> (String, String, String)
        showA (q:qs) [] _ _ _ _ _ = error ("No more states but we still have " ++ (show (q:qs)) ++ " query sequence left")
        showA _ [] i _ oh om oq = ( reverse $ map toLower oh
                                  , reverse om
                                  , reverse $ map toUpper oq
                                  )
        showA [] (p:ps) i lastp oh om oq
          | p == Del = showA [] ps (i+1) lastp ((model i):oh) (' ':om) ('-':oq)
          | otherwise = error $ (show p) ++ " is not a delete state"
        showA (q:qs) (p:ps) i lastp oh om oq -- 'i' should be the node number
          | p == Mat = showA qs ps (i+1) p ((model i):oh) ('|':om) (q:oq)
          | p == BMat = showA qs ps (i+1) p ((model i):oh) ('B':om) (q:oq)
          | p == Ins || p == Beg || p == End = 
              showA qs ps i p ('-':oh) (' ':om) (q:oq)
          | p == Del = showA (q:qs) ps (i+1) p ((model i):oh) (' ':om) ('-':oq)

        -- Strictly for cutting up the strings and displaying them nicely.
        -- Note: Assumes each string is in the correct order and that
        --       each string is of the same length.
        niceify :: (String, String, String) -> String
        niceify (oh, om, oq) = niceify' (splitAt len oh)
                                        (splitAt len om)
                                        (splitAt len oq)
          where niceify' (oh, []) (om, []) (oq, []) = 
                  oh ++ "\n" ++ om ++ "\n" ++ oq
                niceify' (oh, oh') (om, om') (oq, oq') =
                  oh ++ "\n" ++ om ++ "\n" ++ oq 
                  ++ "\n\n"
                  ++ niceify' (splitAt len oh') 
                              (splitAt len om') 
                              (splitAt len oq')

