{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

import Beta
import HmmPlus

data SmurfArgs = SmurfArgs { hmmPlusFile :: FilePath }
  deriving (Show, Data, Typeable)

smurfargs = SmurfArgs { hmmPlusFile = def &= typ "HMM Plus file" &= argPos 0 }

main = do sargs <- cmdArgs smurfargs
          (header, hmm, md) <- parse $ hmmPlusFile sargs
          putStrLn $ show $ getBetaStrands header
          

