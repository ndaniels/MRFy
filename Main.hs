module Main where

import Beta
import HmmPlus

main = do (header, hmm, md) <- parse "test.hmm+"
          putStrLn $ show $ getBetaStrands header
          

