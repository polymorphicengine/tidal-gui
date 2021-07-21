module Parse where

import Text.Parsec.String
import Text.Parsec
import Control.Monad (void, liftM)
import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint
import Data.List
import Text.Parsec.Prim

import Sound.Tidal.Utils

data Command = D Int String | Hush | Cps Double deriving Show

data Block = Block {bStart :: Int
                   ,bEnd :: Int
                   ,bContent ::String
                   } deriving Show

--parsing commands

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

--parsing blocks

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

blocks :: [[(Int,String)]] -> [Block]
blocks [] = []
blocks (b:bs) = (Block {bStart = (fst . head) b , bEnd = (fst . last) b, bContent = concatMap snd b}):(blocks bs)

getBlock :: Int -> [Block] -> Maybe Block
getBlock num [] = Nothing
getBlock num ( block@(Block n1 n2 b):bs) = if n1 <= num && num <= n2 then Just block else getBlock num bs

getBlocks :: String -> [Block]
getBlocks = blocks . blocks' . linesNum

addNewLine :: [String] -> [String]
addNewLine [x] = [x]
addNewLine (x:xs) = (x ++ "\n") : (addNewLine xs)

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `liftM` getParserState

sParser :: Parser ()
sParser = do
        char 's'
        whitespace
        char '"'
        return ()

soundParser :: Parser ()
soundParser = do
        string "sound"
        whitespace
        char '"'
        return ()

deltaMany :: Parser String
deltaMany = do
    x <- manyTill anyChar (try $ (try soundParser <|> sParser))
    s <- sourcePos
    end <- manyTill anyChar (try $ char '\"')
    return $ x ++ "s (deltaContext " ++ show (sourceColumn s - 2) ++ " " ++ show (sourceLine s - 1) ++ " \"" ++ end ++ "\")"

deltaMiniParse :: Parser String
deltaMiniParse = do
          d <- many $ try deltaMany
          rest <- many anyChar
          return $  (concat d) ++ rest

-- since parsec counts tabs as 6 chars, just replace them by a space first
replaceTabs :: String -> String
replaceTabs "" = ""
replaceTabs ('\t':xs) = ' ':replaceTabs xs
replaceTabs (x:xs) = x:replaceTabs xs

-- deltaMini' breaks a lot of things :(
deltaMini':: String -> Either ParseError String
deltaMini' s = parse deltaMiniParse "" (replaceTabs s)
