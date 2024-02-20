module Parser.Terms where

import Parser.Definition
import Parser.Lexer
import Parser.Symbols
import Parser.Keywords
import Untyped.Syntax

import Text.Megaparsec
import Text.Megaparsec.Char

parseTerm :: Parser Term
parseTerm = parseMu <|> parseXtor <|> parseXCase <|> parseShift <|> parseLam <|> parseVar

parseVar :: Parser Term 
parseVar = Var <$> some alphaNumChar

parseMu :: Parser Term
parseMu = do
  parseKeyword KwMu 
  space
  v <- some alphaNumChar
  parseSymbol SymDot
  space
  Mu v <$> parseCommand

parseXtor :: Parser Term
parseXtor = do
  nm <- some alphaNumChar
  sc
  parseSymbol SymBrackO
  sc
  args <- parseTerm `sepBy` (parseSymbol SymComma >> sc)
  sc
  parseSymbol SymBrackC
  return (Xtor nm args)

parsePattern :: Parser Pattern 
parsePattern = do 
  nm <- some alphaNumChar
  sc 
  parseSymbol SymBrackO
  args <- some alphaNumChar `sepBy` (parseSymbol SymComma >> sc)
  sc 
  parseSymbol SymEq 
  parseSymbol SymAngC
  c <- parseCommand
  parseSymbol SymBrackC
  return $ MkPattern nm args c

parseXCase :: Parser Term
parseXCase = do 
  parseKeyword KwCase
  sc
  parseSymbol SymBrackO
  sc
  pts <- parsePattern `sepBy` (parseSymbol SymComma >> sc)
  sc
  parseSymbol SymBrackC
  sc
  return (XCase pts)

parseShift :: Parser Term 
parseShift = do 
  parseSymbol SymBrackO
  t <- parseTerm
  parseSymbol SymBrackC
  return (Shift t)

parseLam :: Parser Term
parseLam = do 
  parseKeyword KwLam
  sc
  parseSymbol SymBrackO
  sc
  v <- some alphaNumChar
  sc
  parseSymbol SymBrackC
  sc
  parseSymbol SymDot
  sc
  Lam v <$> parseCommand

parseCommand :: Parser Command
parseCommand = do 
  parseSymbol SymAngO
  t <- parseTerm
  parseSymbol SymBar
  pol <- parsePol
  parseSymbol SymBar
  u <- parseTerm
  parseSymbol SymAngC
  return (Cut t pol u)
