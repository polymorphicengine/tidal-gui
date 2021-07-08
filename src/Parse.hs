module Parse where

import Text.Parsec.String
import Text.Parsec
import Control.Monad (void)
import Sound.Tidal.Context
import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint

data Command = D Int String | Hush | Cps Double deriving Show

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parsePat :: Parser Command
parsePat = do
        whitespace
        char 'd'
        d <- many1 digit
        whitespace
        char '$'
        pat <- many anyToken
        return $ D (read d) pat

parseHush :: Parser Command
parseHush = do
        whitespace
        string "hush"
        return Hush

parseCps :: Parser Command
parseCps = do
            whitespace
            string "setcps "
            d <- parseDouble
            return $ Cps d

parseDoubleWDot :: Parser Double
parseDoubleWDot = do
        is <- many1 digit
        char '.'
        js <- many1 digit
        return (read $ is ++ "." ++ js ::Double)

parseDoubleWODot :: Parser Double
parseDoubleWODot = do
        is <- many1 digit
        return (read is ::Double)

parseDouble :: Parser Double
parseDouble = try parseDoubleWDot <|> parseDoubleWODot

parseCommand :: Parser Command
parseCommand = try parsePat <|> try parseHush <|> parseCps
