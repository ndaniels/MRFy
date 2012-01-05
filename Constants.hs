module Constants where
import qualified Data.Vector as V  

maxProb = 10e1024 :: Double
amino = "ACDEFGHIKLMNPQRSTVWY"
nucleotide = "ACTG"
numAlphabetAdditions = 1 :: Int -- just X for now

bgFreqs = V.fromList [
                        0.0787945,		-- A
                        0.0151600,		-- C
                        0.0535222,		-- D
                        0.0668298,		-- E
                        0.0397062,		-- F
                        0.0695071,		-- G
                        0.0229198,		-- H
                        0.0590092,		-- I
                        0.0594422,		-- K
                        0.0963728,		-- L
                        0.0237718,		-- M
                        0.0414386,		-- N
                        0.0482904,		-- P
                        0.0395639,		-- Q
                        0.0540978,		-- R
                        0.0683364,		-- S
                        0.0540687,		-- T
                        0.0673417,		-- V
                        0.0114135,		-- W
                        0.0304133,		-- Y
                        1				-- X
                    ]

