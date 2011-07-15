module Extended ( Compose(..)
                , Target(..)
                , Keysym(..)
                , file
                ) where

import Text.ParserCombinators.Parsec
import Data.Char(chr, digitToInt, isSeparator, isAlphaNum)

data Compose = SeqDef [Keysym] Target
               deriving (Show)

data Target = Output String (Maybe Keysym)
            | Group [Compose]
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
           char '{'
           g <- group
           char '}'
           whiteSpace
           return (Group g)
         <|>
         do
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
