module ParsecHelper
  ( eol
  , oneStringOf
  , fractional
  , decimal
  , lexeme
  , optSpaces
  , reqSpaces
  )
where
    
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.String
import Control.Applicative
import Numeric
import qualified Text.Parsec.Token as P
import Data.Char
import Data.Either
import Data.Maybe (catMaybes)





eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "eol"

-- Parsec's spaces function is a many1, not a many. Requires at least 1 space.
optSpaces :: GenParser Char st String
optSpaces = many (oneOf " \t")

reqSpaces :: GenParser Char st String
reqSpaces = many1 (oneOf " \t")


oneStringOf :: [String] -> GenParser Char st String
oneStringOf = choice . map (try . string)

fractional :: Fractional a => Parser a
fractional = try $ do
  n <- fromIntegral <$> decimal
  char '.'
  f <- foldr (\d f -> (f + fromIntegral (digitToInt d))/10.0) 0.0 <$> many1 digit  
  return $ n + f

decimal :: Parser Int
decimal = foldl (\n d -> 10 * n + digitToInt d) 0 <$> many1 digit

lexeme :: Parser a -> Parser a
lexeme p = p <* skipMany (char ' ')
