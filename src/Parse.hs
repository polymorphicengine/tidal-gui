module Parse where

import Text.Parsec.String
import Text.Parsec
import Control.Monad (void, liftM)
import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint
import Data.List
import Text.Parsec.Prim

data Command = D Int String | Hush | Cps Double deriving Show

data Block = Block {bStart :: Int
                   ,bEnd :: Int
                   ,bContent ::String
                   }

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

parseDoubleWP :: Parser Double
parseDoubleWP = do
        is <- many1 digit
        char '.'
        js <- many1 digit
        return (read $ is ++ "." ++ js ::Double)

parseDoubleWOP :: Parser Double
parseDoubleWOP = do
        is <- many1 digit
        return (read is ::Double)

parseDouble :: Parser Double
parseDouble = try parseDoubleWP <|> parseDoubleWOP

parseCommand :: Parser Command
parseCommand = try parsePat <|> try parseHush <|> parseCps

whiteString :: String -> Bool
whiteString "" = True
whiteString (x:xs) = if elem x " \t\n" then whiteString xs else False

linesNum :: String -> [(Int,String)]
linesNum s = zip [0..] (addNewLine . lines $ s)

blocks' :: [(Int,String)] -> [[(Int,String)]]
blocks' ss = case break (whiteString . snd) ss of
              ([], (y:ys)) -> blocks' ys
              (xs, (y:ys)) -> xs:(blocks' ys)
              (xs, []) -> [xs]

blocks :: [[(Int,String)]] -> [(Int,Int,String)]
blocks [] = []
blocks (b:bs) = ((fst . head) b, (fst . last) b, concatMap snd b) : (blocks bs)

getBlock :: Int -> [(Int, Int, String)] -> Maybe (Int, String)
getBlock num [] = Nothing
getBlock num ( (n1, n2, b):bs) = if n1 <= num && num <= n2 then Just (n1,b) else getBlock num bs

getBlocks :: String -> [(Int,Int,String)]
getBlocks = blocks . blocks' . linesNum

addNewLine :: [String] -> [String]
addNewLine [x] = [x]
addNewLine (x:xs) = (x ++ "\n") : (addNewLine xs)
