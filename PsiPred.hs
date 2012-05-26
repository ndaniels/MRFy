module PsiPred 
  ( parsePsiPred
  , checkPreds
  , SSPrediction(..)
  , PsiPred(..)
  ) 
where

import ParsecHelper
import Constants

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.String
import Control.Applicative
import Numeric
import qualified Text.Parsec.Token as P
import Data.Char
import Data.Either

checkPreds :: Either ParseError [SSPrediction] -> [SSPrediction]
checkPreds (Right s) = s
checkPreds (Left e)  = error "Failed to parse PsiPred file"


parsePsiPred :: String -> IO (Either ParseError [SSPrediction])
parsePsiPred fName = parseFromFile psiPredFile fName -- this should pull out of Either
-- error here or return the MRF

psiPredFile :: GenParser Char () [SSPrediction]
psiPredFile = do -- 
            comm  <- commentLine
            blankLine
            ents  <- many predLine
            optional eol
            eof
            return ents

data PsiPred = PsiPred { header::String
                       , ss_predictions::[SSPrediction]
                       }
                       deriving (Show)

data SSPrediction = SSPrediction { residueNum::Int
                                 , residueLetter::Char
                                 , ss_prediction::SecondaryStructure
                                 , turn_score::Double
                                 , alpha_score::Double
                                 , beta_score::Double
                                 }
                                 deriving (Show)

data SecondaryStructure = Alpha
                        | Beta
                        | Turn
                        deriving (Show)

mkSecondary :: Char -> SecondaryStructure
mkSecondary 'H' = Alpha
mkSecondary 'E' = Beta
mkSecondary 'C' = Turn
mkSecondary  _  = error "Invalid Secondary Structure"

commentLine = string "# " *> manyTill (noneOf "\r\n") eol

blankLine = optSpaces *> eol

predLine = optSpaces *> (SSPrediction                                 <$>
                         decimal                                      <*>
                         (reqSpaces *> oneOf aminoList)               <*>
                         (mkSecondary <$> (reqSpaces *> oneOf "HEC")) <*>
                         (reqSpaces *> fractional)                    <*>
                         (reqSpaces *> fractional)                    <*>
                         (reqSpaces *> fractional)
                        ) <* eol
