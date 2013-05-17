{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Main where

import System.Environment

import CommandArgs
import FileOps
import V4

main = do argv <- getArgs
          runCommand (getOpts argv)
