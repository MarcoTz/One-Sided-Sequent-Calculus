module Parser.Terms where

import Parser.Definition
import Parser.Lexer
import Parser.Symbols
import Parser.Keywords
import Untyped.Syntax
import Common

import Text.Megaparsec
import Text.Megaparsec.Char

import Pretty.Terms ()

parseTerm :: Parser Term
parseTerm = parseMu <|> parseXCase <|> parseShift <|> parseLam <|> try parseXtor <|> try parseVar

parseVar :: Parser Term 
parseVar = do 
  v <- some alphaNumChar
  return $ Var v

parseMu :: Parser Term
parseMu = do
  parseKeyword KwMu <|> parseKeyword Kwmu 
  sc
  v <- some alphaNumChar
  sc
  parseSymbol SymDot
  sc
  Mu v <$> parseCommand

parseXtor :: Parser Term
parseXtor = do
  nm <- some alphaNumChar
  sc
  Xtor nm <$> parseXtorArgs

parseXtorArgs :: Parser [Term]
parseXtorArgs = (do
  parseSymbol SymParensO
  sc
  args <- parseTerm `sepBy` (parseSymbol SymComma >> sc)
  sc
  parseSymbol SymParensC
  return args)
  <|>
  return []

parsePatternVars :: Parser [Variable]
parsePatternVars = (do 
  parseSymbol SymParensO
  args <- some alphaNumChar `sepBy` (parseSymbol SymComma >> sc)
  parseSymbol SymParensC 
  return args)
  <|>
  return []

parsePattern :: Parser Pattern 
parsePattern = do 
  nm <- some alphaNumChar
  sc
  args <- parsePatternVars 
  sc
  parseSymbol SymEq 
  parseSymbol SymAngC
  sc
  MkPattern nm args <$> parseCommand

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
parseCommand = (do 
  parseSymbol SymAngO
  sc
  t <- parseTerm
  sc
  parseSymbol SymBar
  sc
  pol <- parsePol
  sc
  parseSymbol SymBar
  sc
  u <- parseTerm
  sc
  parseSymbol SymAngC
  return (Cut t pol u))
  <|>
  (do 
    parseKeyword KwDone 
    return Done)
