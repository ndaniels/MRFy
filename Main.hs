{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Main where

import System.Environment

import CommandArgs
import FileOps
import V2() -- trigger compilation

main = do argv <- getArgs
          runCommand (getOpts argv)
