module Parser.Types where 

import Parser.Definition
import Parser.Symbols
import Parser.Lexer
import Untyped.Program
import Common

import Text.Megaparsec
import Text.Megaparsec.Char

parseTy :: Parser Ty 
parseTy = try parseTyDecl <|> parseTyVar

parseTyDecl :: Parser Ty
parseTyDecl = do
  tyn <- some alphaNumChar
  parseSymbol SymParensO 
  args <- parseTy `sepBy` (parseSymbol SymComma >> space)
  parseSymbol SymParensC
  return (TyDecl tyn args)

parseTyVar :: Parser Ty
parseTyVar = do 
    var <- some alphaNumChar
    return (TyVar var)


parseTyArgs :: Parser [(Variable, Pol)]
parseTyArgs = (do 
  parseSymbol SymParensO
  vars <- parsePolVar `sepBy` (parseSymbol SymComma >> space)
  parseSymbol SymParensC
  return vars)
  <|>
  return []

parsePolVar :: Parser (Variable,Pol)
parsePolVar = do 
  nm <- some alphaNumChar
  parseSymbol SymColon
  pol <- parsePol
  return (nm,pol)
