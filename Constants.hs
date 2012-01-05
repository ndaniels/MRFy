module Constants where

import Data.Vector

type Alphabet = Vector Char
  
maxProb = 10e1024 :: Double
aminoS = "ACDEFGHIKLMNPQRSTVWY"
nucleotideS = "ACTG"

amino = fromList aminoS :: Alphabet
nucleotide = fromList nucleotideS

getResidue :: Alphabet -> Int -> Char
getResidue alpha i = alpha ! i

