module Extended ( Compose(..)
                , Target(..)
                , Keysym(..)
                , file
                ) where

import Text.ParserCombinators.Parsec
import Data.Char(chr, digitToInt, isSeparator, isAlphaNum)

data Compose = Def Trigger Target
               deriving (Eq, Show)

data Trigger = KeySeq [Keys]
             | Name String
               deriving (Eq, Show)

data Target = Output String (Maybe Keysym)
            | Group [Compose]
            | Ref String
              deriving (Eq, Show)

data Keys = Sym Keysym
          | Lit String
            deriving (Eq, Show)

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
         option Nothing (fmap Just def)

def :: Parser Compose
def = do
           trig <- trigger
           char ':'
           whiteSpace
           targ <- target
           return (Def trig targ)

trigger :: Parser Trigger
trigger = do
            char '['
            n <- name
            char ']'
            whiteSpace
            return (Name n)
          <|>
          fmap KeySeq (many keys)

keys :: Parser Keys
keys = fmap Sym key <|> fmap Lit stringLiteral

key :: Parser Keysym
key = do
        char '<'
        k <- name
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
           char '@'
           n <- name
           return (Ref n)
         <|>
         do
           str <- stringLiteral
           sym <- option Nothing (fmap Just name)
           whiteSpace
           return (Output str sym)

name :: Parser Keysym
name = many1 (satisfy (\c -> c == '_' || isAlphaNum c)) <?> "name"

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
