module BetaProps
where
  
import Beta
import MRFTypes

import Test.QuickCheck

betaProps :: [(String, Property)]
betaProps = [ ("genStrands", property genStrands)
            ]

genStrands :: [StrandPair] -> Bool
genStrands sps = len >= 0
  where len = length $ getBetaStrands sps

