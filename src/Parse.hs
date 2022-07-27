module Parse where

import Text.Parsec.String
import Text.Parsec
import Control.Monad (void)

type Position = (Int,Int)

data Configurable = DefPath

instance Show Configurable where
  show DefPath = "defPath"

data Command = Hush
             | T String
             | M String
             | Statement String
             | Conf Configurable String
             | Listen String Int
             deriving Show

data Block = Block {bStart :: Int
                   ,bEnd :: Int
                   ,bContent ::String
                   } deriving Show

--parsing commands


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parseHush :: Parser Command
parseHush = do
        whitespace
        _ <- string "hush"
        return Hush

parseType :: Parser Command
parseType = do
        whitespace
        _ <- string ":t"
        s <- many anyChar
        return (T s)

parseMakro :: Parser Command
parseMakro = do
        whitespace
        _ <- string ":m"
        s <- many anyChar
        return (M s)

parseSetDefPath :: Parser Command
parseSetDefPath = do
        whitespace
        _ <- string ":set"
        whitespace
        _ <- string "defPath"
        whitespace
        s <- many anyChar
        return (Conf DefPath s)

parseListen :: Parser Command
parseListen = do
        whitespace
        _ <- string ":listen"
        whitespace
        s <- many (letter <|> digit <|> char '.')
        _ <- char ':'
        i <- fmap read $ many1 digit
        return (Listen s i)

parseStatement :: Parser Command
parseStatement = fmap Statement $ many anyChar

parseCommand :: Parser Command
parseCommand = try parseHush <|> try parseType <|> try parseMakro <|> try parseSetDefPath <|> try parseListen <|> parseStatement

--parsing blocks

whiteString :: String -> Bool
whiteString "" = True
whiteString (x:xs) = if elem x " \t\n" then whiteString xs else False

linesNum :: String -> [(Int,String)]
linesNum s = zip [0..] (addNewLine . lines $ s)

blocks' :: [(Int,String)] -> [[(Int,String)]]
blocks' ss = case break (whiteString . snd) ss of
              ([], (_:ys)) -> blocks' ys
              (xs, (_:ys)) -> xs:(blocks' ys)
              (xs, []) -> [xs]

blocks :: [[(Int,String)]] -> [Block]
blocks [] = []
blocks ([]:_) = []
blocks (b:bs) = (Block {bStart = (fst . head) b , bEnd = (fst . last) b, bContent = concatMap snd b}):(blocks bs)

getBlock :: Int -> [Block] -> Maybe Block
getBlock _ [] = Nothing
getBlock num ( block@(Block n1 n2 _):bs) = if n1 <= num && num <= n2 then Just block else getBlock num bs

getBlocks :: String -> [Block]
getBlocks = blocks . blocks' . linesNum

addNewLine :: [String] -> [String]
addNewLine [] = []
addNewLine [x] = [x]
addNewLine (x:xs) = (x ++ "\n") : (addNewLine xs)

getLineContent :: Int -> [(Int,String)] -> Maybe Block
getLineContent _ [] = Nothing
getLineContent num ((n,s):ls) | n == num = Just $ Block n n s
                              | otherwise = getLineContent num ls

-------

replaceTabs :: String -> String
replaceTabs "" = ""
replaceTabs ('\t':xs) = "    " ++ replaceTabs xs
replaceTabs (x:xs) = x : replaceTabs xs
