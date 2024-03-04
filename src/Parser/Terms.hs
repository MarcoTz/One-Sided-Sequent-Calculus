module Parser.Terms where

import Parser.Definition
import Parser.Lexer
import Parser.Symbols
import Parser.Keywords
import Syntax.Parsed.Terms
import Common

import Text.Megaparsec

-- Xtors with no arguments are parsed as variables
parseTerm :: Parser Term
parseTerm = parseMu <|> parseXCase <|> parseShift <|> parseLam <|> try parseXtor <|> parseVar

parseVar :: Parser Term 
parseVar = Var <$> parseVariable 

parseMu :: Parser Term
parseMu = do
  parseKeyword KwMu <|> parseKeyword Kwmu 
  sc
  v <- parseVariable 
  sc
  parseSymbol SymDot
  sc
  Mu v <$> parseCommand

parseXtor :: Parser Term
parseXtor = do
  nm <- parseXtorName 
  sc
  parseSymbol SymParensO
  sc
  args <- parseTerm `sepBy` (parseSymbol SymComma >> sc)
  sc
  parseSymbol SymParensC 
  return $ Xtor nm args

parsePatternVars :: Parser [Variable]
parsePatternVars = (do 
  parseSymbol SymParensO
  args <- parseVariable `sepBy` (parseSymbol SymComma >> sc)
  parseSymbol SymParensC 
  return args)
  <|>
  return []

parsePattern :: Parser Pattern 
parsePattern = do 
  nm <- parseXtorName 
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
  v <- parseVariable 
  sc
  parseSymbol SymBrackC
  sc
  parseSymbol SymDot
  sc
  Lam v <$> parseCommand


parseCommand :: Parser Command 
parseCommand = parseCut <|> parseDone

parseCut :: Parser Command
parseCut = do 
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
  return (Cut t pol u)

parseDone :: Parser Command
parseDone = parseKeyword KwDone  >> return Done

