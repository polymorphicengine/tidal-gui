module Parse where

import Text.Parsec.String
import Text.Parsec
import Control.Monad (void)
import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint
import Data.List

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


whiteString :: String -> Bool
whiteString "" = True
whiteString (x:xs) = if elem x " \t" then whiteString xs else False

linesNum :: String -> [(Int,String)]
linesNum s = zip [0..] (lines s)

blocks' :: [(Int,String)] -> [[(Int,String)]]
blocks' ss = case break (whiteString . snd) ss of
              ([], (y:ys)) -> blocks' ys
              (xs, (y:ys)) -> xs:(blocks' ys)
              (xs, []) -> [xs]

blocks :: [[(Int,String)]] -> [(Int,Int,String)]
blocks [] = []
blocks (b:bs) = ((fst . head) b, (fst . last) $ b, concatMap snd b) : (blocks bs)

getBlock :: Int -> [(Int, Int, String)] -> Maybe String
getBlock num [] = Nothing
getBlock num ( (n1, n2, b):bs) = if n1 <= num && num <= n2 then Just b else getBlock num bs

getBlocks :: String -> [(Int,Int,String)]
getBlocks = blocks . blocks' . linesNum
