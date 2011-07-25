module Extended ( Compose(..)
                , Target(..)
                , Keysym(..)
                , file
                , standardize
                ) where

import Text.ParserCombinators.Parsec
import Data.Char (ord, chr, digitToInt, isSeparator, isAlphaNum, toUpper)
import Data.List (partition)
import Numeric (showHex)

import qualified XCParser as X

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


standardize :: [Compose] -> [X.Compose]
standardize cs = concatMap (flattenDef (map pairize nameDefs)) seqDefs
    where (nameDefs, seqDefs) = partition isNameDef cs
          isNameDef (Def (Name _) _) = True
          isNameDef _ = False

flattenDef :: [(String, Target)] -> Compose -> [X.Compose]
flattenDef _ (Def (Name _) _) = error "flattenDef: got a nameDef instead of a seqDef"
flattenDef nameDefs (Def (KeySeq ks) targ) = case unRef targ of
                                                 (Ref _) -> error "flattenDef: got a Ref from unRef"
                                                 (Group cs) -> map (prependKeys (concatMap flattenKeys ks)) $ concatMap (flattenDef nameDefs) cs
                                                 (Output str m) -> [X.SeqDef (concatMap flattenKeys ks) $ X.Output str m]
    where unRef (Ref n) = unRef $ maybe (error "unRef: undefined reference") id $ lookup n nameDefs
          unRef d = d

prependKeys :: [X.Keysym] -> X.Compose -> X.Compose
prependKeys ks (X.SeqDef kss targ) = X.SeqDef (ks ++ kss) targ

flattenKeys :: Keys -> [X.Keysym]
flattenKeys (Sym k) = [k]
flattenKeys (Lit str) = map charToSym str

charToSym :: Char -> X.Keysym
charToSym c = 'U' : padToWith 4 '0' (map toUpper (showHex (ord c) []))

pairize :: Compose -> (String, Target)
pairize (Def (Name n) targ) = (n, targ)
pairize _ = error "pairize: got a non-nameDef"

padToWith :: Int -> a -> [a] -> [a]
padToWith n a l = replicate (n - length l) a ++ l

