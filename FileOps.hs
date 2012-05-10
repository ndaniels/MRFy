module FileOps
       ( loadTestData, translateQuery
       )
where
  
import qualified Data.Vector as V

import Bio.Sequence
import Bio.Sequence.Fasta

import Constants
import CommandArgs
import HmmPlus
import Viterbi

loadTestData :: Files -> IO (HMM, [QuerySequence])
loadTestData files =
  do querySeqs <- readFasta $ fastaF files
     (_, hmm, _) <- parse $ hmmPlusF files
     return (hmm, map (translateQuery . toStr . seqdata) querySeqs)
     
translateQuery :: String -> V.Vector Int
translateQuery = V.fromList . map lookup
  where lookup k = case V.elemIndex k Constants.amino of
                        Just i -> i
                        Nothing -> error "Residue not found in alphabet"


