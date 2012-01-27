module Constants where

import Data.Vector

type Alphabet = Vector Char
  
maxProb = 10e1024 :: Double
amino = fromList "ACDEFGHIKLMNPQRSTVWYX"
nucleotide = fromList "ACTG"
betaCoeff = 0.5
generations = 1000 :: Int

-- amino = fromList aminoS :: Alphabet 
-- nucleotide = fromList nucleotideS 

getResidue :: Alphabet -> Int -> Char
getResidue alpha i = alpha ! i

numAlphabetAdditions = 1 :: Int -- just X for now

data Debugging = Debugging { slicing :: Bool }
debug = Debugging { slicing = False }

