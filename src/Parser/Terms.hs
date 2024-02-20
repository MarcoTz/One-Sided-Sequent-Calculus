module Parser.Terms where

import Parser.Definition
import Parser.Lexer
import Parser.Symbols
import Parser.Keywords
import Untyped.Syntax

import Text.Megaparsec
import Text.Megaparsec.Char

parseTerm :: Parser Term
parseTerm = return $ Var "x"

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
