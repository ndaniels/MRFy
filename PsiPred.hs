{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables, NamedFieldPuns #-}

module Smurf.PsiPred where
import Language.Pads.Padsc
import Language.Pads.GenPretty
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

ws = REd "[\t\n\r ]+|$" " "

amino = "ACDEFGHIKLMNPQRSTVWY"

[pads|
  data PsiPredFile = PsiPredFile { header::PsiPredHeader, ws, BlankLine, ss_predictions::[Line SSPrediction] terminator EOF }
  
  type BlankLine = (ws, EOR)
  type PsiPredHeader = ('#', ws, StringLn, EOR)
  
  data SSPrediction = SSPrediction { ws,
                      residueNum::Int, ws,
                      residueLetter::Letter, ws,
                      ss_prediction::SecondaryStructure, ws,
                      turn_score::Double, ws,
                      alpha_score::Double, ws,
                      beta_score::Double
  }
  
  data SecondaryStructure = Alpha 'H'
                          | Beta 'E'
                          | Turn 'C'
                          
  type Letter = constrain c::Char where <| c `elem` amino |>                        

|]

parse :: FilePath -> IO ([SSPrediction], PsiPredFile_md)
parse f = do (PsiPredFile header ss_pred, md) <- parseFile f
             return (ss_pred, md)

