{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables, NamedFieldPuns,
             NoMonomorphismRestriction #-}
module HMMPlus
       ( parseMRF
       , checkMRF
       )
where

import Data.Ix
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.String
import Control.Applicative
import Numeric
import qualified Text.Parsec.Token as P
import Data.Char
import Data.Either
import MRFTypes
import Beta
import Debug.Trace (trace)


import Data.Maybe (catMaybes)

import Score
import ParsecHelper

mrfVersion = "HMMER3/a" -- version of MRF file to support
supportedAlphabet = "amino"



-- many headerlines, then HMM literal, then many HMM line-triples
-- actually it's a header, which is many headerlines
-- terminated by 'HMM'
-- then it's an HMM, which is the special 0 node, followed by many HMMNodes
-- 

-- parse mean, rmsd, leng, version as special cases?
-- that would make it easy to pass them to the HMMHeader constructor.


checkMRF :: Either ParseError MRF -> MRF
checkMRF (Right m) = m
checkMRF (Left e)  = error ("Failed to parse HMMPlus file: " ++ show e)

parseMRF :: String -> IO (Either ParseError MRF)
parseMRF fName = parseFromFile mrfFile fName -- this should pull out of Either
-- error here or return the MRF

mrfFile :: GenParser Char () MRF
mrfFile = do -- 
        ver <- versionLine
        hs  <- headerLines
        ss  <- ssSection -- secondary structure
        eoH -- 'HMM'
        transHeader -- m->m junk
        hmmList <- many hmmNodeGroup
        string "//"
        optional eol
        eof
        return MRF {
                     hmmHeader = mkHeader ver hs ss
                   , hmm = V.fromList hmmList
                   }

mkHeader (vMajor, vMinor) hs ss = HMMHeader { betas       = getBetaStrands ss
                                            , modelLength = checkLeng $ hmmLeng hs
                                            , modelAlpha  = trace (show hs) $ checkAlpha $ hmmAlph hs
                                            , modelName   = hmmName hs
                                            , modelVers   = checkVersion vMajor vMinor
                                            , modelMean   = hmmMean hs
                                            , modelStdDev = hmmSD hs
                                            }


checkAlpha a = case a of
                 Nothing -> error "No alphabet"
                 Just v -> if v == supportedAlphabet then v
                           else error "Unsupported alphabet " ++ v

checkLeng l = case l of
                Nothing -> error "No model length"
                Just x  -> x

checkVersion vMajor vMinor | vMajor == mrfVersion = vMajor ++ vMinor
                           | otherwise            = error "Unsupported version "
                                                    ++ vMajor ++ " " ++ vMinor

versionLine = (,) <$> ((++) <$> string "HMMER" <*> many1 (noneOf " ")) 
                  <*> manyTill (noneOf "\r\n") eol

-- data HMMHeader = HMMHeader { betas       :: [StrandPair] -- TODO add alphas (sstructs)
--                            , modelLength :: Int
--                            , modelAlpha  :: String
--                            , modelName   :: String
--                            , modelVers   :: String
--                            , modelMean   :: Maybe Double
--                            , modelStdDev :: Maybe Double
--                            }


-- we really only want NAME, LENG, ALPH, MEAN, RMSD
headerLines = many headerLine
headerLine = (,) <$> headerKey <*> (reqSpaces *> manyTill (noneOf "\r\n") eol)

hmmName :: [(String, String)] -> Maybe String
hmmName = lookup "NAME"

hmmAlph :: [(String, String)] -> Maybe String
hmmAlph = lookup "ALPH"

hmmLeng :: [(String, String)] -> Maybe Int
hmmLeng vs = fmap (read :: String -> Int) (lookup "LENG" vs)

hmmSD :: [(String, String)] -> Maybe Double
hmmSD vs = fmap (read :: String -> Double) (lookup "RMSD" vs)

hmmMean :: [(String, String)] -> Maybe Double
hmmMean vs = fmap (read :: String -> Double) (lookup "MEAN" vs)


betaLine = string "BETA " *> (StrandPair <$> p_int_sp <*> p_int_sp 
                         <*> p_int_sp <*> (p_int_sp *> p_direction) 
                         <*> p_exposure) <* eol

alphaLine = string "ALPHA " *> (Helix <$> p_int_sp <*> p_int_sp) 
                            <* eol -- this is not yet right

p_int_sp = liftA (read :: String -> Int) (optSpaces *> many1 digit <* optSpaces <?> "int")

p_int = decimal

p_direction = liftA mkDirection (optSpaces *> dir <* optSpaces <?> "direction")
            where dir = oneStringOf [ "1", "-1" ]

p_exposure = liftA (map mkExposure) 
                       (optSpaces *> many1 (oneOf "io") <* optSpaces <?> "exposure")



ssSection = many ssLine

ssLine = betaLine -- <|> alphaLine -- not sure how to get this to work

-- this is fragile, but it's also not important data
transHeader = optSpaces *> string "m->m     m->i     m->d     i->m     i->i     d->m     d->d" <* eol


-- handle 3 kinds of lines, as a triple
-- also seperately handle the node 0 lines

-- nodenum, matchEmissions
-- insertionEmissions
-- transitions
hmmNodeGroup = HMMNode <$> hmmNodeNum <*>hmmMatchEmissions <*> hmmInsertionEmissions <*>
                           hmmTransitions

-- this is SOMETHING to an Int. 
hmmNodeNum = optSpaces *> (try (const 0 <$> string "COMPO") <|> p_int) <* optSpaces 
           -- where comp = optional (string "COMPO") -- string "COMP" <|> string "" -- ugly. better way?

junk = lexeme decimal <* count 2 (lexeme $ char '-')

hmmMatchEmissions     = optSpaces *> (U.fromList <$> many1 (lexeme logProb)) 
                      <* optional junk <* eol
                      <?> "matchEmissions"

hmmInsertionEmissions = optSpaces *> (U.fromList <$> many1 (lexeme logProb)) <* eol 
                      <?> "insertionEmissions"

hmmTransitions        = optSpaces *> 
                       (
                       mkTransScores <$> 
                       (lexeme logProb <* optSpaces) <*>
                       (lexeme logProb <* optSpaces) <*>
                       (lexeme logProb <* optSpaces) <*>
                       (lexeme logProb <* optSpaces) <*>
                       (lexeme logProb <* optSpaces) <*>
                       (lexeme logProb <* optSpaces) <*>
                       (lexeme logProb <* optSpaces)
                       ) 
                       <* eol 
                      <?> "hmmTransitions"

headerKey = oneStringOf [ "NAME"
                        , "LENG"
                        , "ALPH"
                        , "MEAN"
                        , "RMSD"
                        ,  "RF"
                        , "CS"
                        , "MAP"
                        , "DATE"
                        , "NSEQ"
                        , "EFFN"
                        , "CKSUM"
                        , "STATS"
                        , "DESC", "SMURFLITE", "SIMEVFREQ", "SIMEVCOUNT"
                        ]

headerPair :: GenParser Char st (String, String)
headerPair = do manyTill anyChar . try $ lookAhead headerKey
                name <- many1 $ noneOf " "
                string " "
                val <- manyTill (noneOf "\r\n") $ eol
                return (name, val)


logProb :: Parser Score
logProb = (Score <$> fractional) <|> (negLogZero <$ char '*') <?> "logProb"




eoH = string "HMM" <* manyTill anyChar eol <?> "hmmSeparator"
