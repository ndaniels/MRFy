{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Main where

import System.Environment

import CommandArgs
import FileOps

main = do argv <- getArgs
          runCommand (getOpts argv)
