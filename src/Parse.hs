module Parse where

import Text.Parsec.String
import Text.Parsec
import Control.Monad (void)

type Position = (Int,Int)

data Command = H String String Position
             | Hush
             | T String
             | Other String
             | Def String  deriving Show

data Block = Block {bStart :: Int
                   ,bEnd :: Int
                   ,bContent ::String
                   } deriving Show

--parsing commands


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parseHighlight :: Parser Command
parseHighlight = do
        white1 <- many $ oneOf " \n\t"
        _ <- char 'h'
        h <- many1 digit
        white3 <- many $ oneOf " \t\n"
        _ <- char '$'
        white4 <- many $ oneOf " \t\n"
        pat <- many anyToken
        case elem '\n' white3 of
          False -> case elem '\n' white4 of
            False -> return $ H ("h"++h) (white4 ++ pat) (0,length white1 + length h + length white3 + 2)
            True -> return $ H ("h"++h) (white4 ++ pat) (0,2)
          True -> case elem '\n' white4 of
            True -> return $ H ("h"++h) (white4 ++ pat) (1,0)
            False -> return $ H ("h"++h) (white4 ++ pat) (1,1)

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

parseOther :: Parser Command
parseOther = fmap Other $ many anyChar

parseDef :: Parser Command
parseDef = do
        l <- string "let"
        s <- many anyChar
        return $ Def $ replaceTabs (l ++ s)

parseCommand :: Parser Command
parseCommand = try parseHighlight <|> try parseHush <|> try parseType <|> try parseDef <|> parseOther

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
