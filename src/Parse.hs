module Parse where

import Text.Parsec.String
import Text.Parsec
import Control.Monad (void)

type Position = (Int,Int)

data Command = H Int String Position | Hush | Cps Double | T String | Other String deriving Show

data Block = Block {bStart :: Int
                   ,bEnd :: Int
                   ,bContent ::String
                   } deriving Show

--parsing commands


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parsePat :: Parser Command
parsePat = do
        white1 <- many $ oneOf " \n\t"
        _ <- char 'h'
        h <- many1 digit
        white2 <- many $ oneOf " \t"
        white3 <- many $ oneOf " \t\n"
        _ <- char '$'
        white4 <- many $ oneOf " \t\n"
        pat <- many anyToken
        case elem '\n' white3 of
          False -> case elem '\n' white4 of
            False -> return $ H (read h) (white4 ++ pat) (0,length white1 + length h + length white2 + length white3 + 2)
            True -> return $ H (read h) (white4 ++ pat) (0,2)
          True -> return $ H (read h) (white4 ++ pat) (1,0)

parseHush :: Parser Command
parseHush = do
        whitespace
        _ <- string "hush"
        return Hush

parseT :: Parser Command
parseT = do
        whitespace
        _ <- string ":t"
        s <- many anyChar
        return (T s)

parseCps :: Parser Command
parseCps = do
            whitespace
            _ <- string "setcps "
            d <- parseDouble
            return $ Cps d

parseDoubleWP :: Parser Double
parseDoubleWP = do
        is <- many1 digit
        _ <- char '.'
        js <- many1 digit
        return (read $ is ++ "." ++ js ::Double)

parseDoubleWOP :: Parser Double
parseDoubleWOP = do
        is <- many1 digit
        return (read is ::Double)

parseDouble :: Parser Double
parseDouble = try parseDoubleWP <|> parseDoubleWOP

parseOther :: Parser Command
parseOther = fmap Other $ many anyChar

parseCommand :: Parser Command
parseCommand = try parsePat <|> try parseHush <|> try parseCps <|> try parseT <|> parseOther

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
