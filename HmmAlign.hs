module HmmAlign where

import Data.Array

import HmmPlus

type QuerySequence = Array Int Char
type Score = Int

hmmAlign :: QuerySequence -> HMM -> Score

