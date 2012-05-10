{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Main where

import System.Environment

import CommandArgs
import FileOps

main = do argv <- getArgs
          case getOpts argv
            of AlignmentSearch searchParams files -> error "ow!"
               TestHmm s -> error "double ow!"
