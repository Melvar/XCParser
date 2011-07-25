module XCParser ( Compose(..)
                , Target(..)
                , Keysym(..)
                , file
                , unparse
                ) where

import Text.ParserCombinators.Parsec
import Data.Char (ord, chr, digitToInt, isSeparator, isAlphaNum)
import Numeric (showOct)

data Compose = SeqDef [Keysym] Target
               deriving (Show)

data Target = Output String (Maybe Keysym)
              deriving (Show)

type Keysym = String

file :: Parser [Compose]
file = do
         g <- group
         eof
         return g

group :: Parser [Compose]
group = do
         lines <- line `sepEndBy` newline
         let composes = foldr (maybe id (:)) [] lines
         return composes

line :: Parser (Maybe Compose)
line = do
         whiteSpace
         option Nothing (fmap Just seqDef)

seqDef :: Parser Compose
seqDef = do
           keys <- many1 key
           char ':'
           whiteSpace
           targ <- target
           return (SeqDef keys targ)

key :: Parser Keysym
key = do
        char '<'
        k <- keysym
        char '>'
        whiteSpace
        return k
      <?> "<keysym_name>"

target :: Parser Target
target = do
           str <- stringLiteral
           sym <- option Nothing (fmap Just keysym)
           whiteSpace
           return (Output str sym)

keysym :: Parser Keysym
keysym = many1 (satisfy (\c -> c == '_' || isAlphaNum c)) <?> "keysym name"

stringLiteral :: Parser String
stringLiteral = do
                  str <- between (char '"') (char '"') (many stringChar)
                  whiteSpace
                  return str
                <?> "string literal"

stringChar :: Parser Char
stringChar = do
               char '\\'
               escapedChar
             <|> noneOf "\"\\"

escapedChar :: Parser Char
escapedChar = oneOf "\\\"'?"
              <|> do
                    c <- oneOf "nrbtfav"
                    return $ maybe '\xfffd' id $ c `lookup` [('n','\n'),('r','\r'),('b','\b'),('t','\t'),('f','\f'),('a','\a'),('v','\v')]
              <|> do
                    char 'x'
                    a <- hexDigit
                    b <- hexDigit
                    return $ chr (16*(digitToInt a) + digitToInt b)
              <|> do
                    a <- octDigit
                    b <- octDigit
                    c <- octDigit
                    return $ chr (8*8*(digitToInt a) + 8*(digitToInt b) + digitToInt c)
              <?> "escaped character"

whiteSpace :: Parser ()
whiteSpace = do
               skipMany (satisfy (\c -> c == '\t' || isSeparator c))
               option () comment
             <?> "whitespace"

comment :: Parser ()
comment = do
            char '#'
            skipMany (satisfy (/='\n'))
          <?> "comment"

unparse :: [Compose] -> String
unparse = unlines . map unparseLine

unparseLine :: Compose -> String
unparseLine (SeqDef ks (Output str m)) = keys ++ " : " ++ targ
    where keys = unwords $ map (('<':) . (++">")) ks
          targ = cString str ++ maybe "" (' ':) m

cString :: String -> String
cString s = '"' : concatMap escape s ++ "\""
    where escape c | c < ' ' = maybe (numescape c) (('\\':) . (:[])) $ lookup c [('\n','n'),('\r','r'),('\b','b'),('\t','t'),('\f','f'),('\a','a'),('\v','v')]
                   | c == '\127' = numescape c
                   | c > '\127' && c < ' ' = "\\302" ++ numescape c
                   | c `elem` "\\\"'?" = ['\\',c]
                   | otherwise = [c]
          numescape c = '\\' : showOct (ord c) []

