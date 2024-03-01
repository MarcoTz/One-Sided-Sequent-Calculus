module Parser.Types where 

import Parser.Definition
import Parser.Symbols
import Parser.Keywords
import Parser.Lexer
import Syntax.Parsed.Types
import Common

import Text.Megaparsec
import Text.Megaparsec.Char


parseTy :: Parser Ty 
parseTy = try parseTyDecl <|> parseTyVar

parseTypeScheme :: Parser TypeScheme 
parseTypeScheme = do 
  parseKeyword KwForall <|> parseKeyword Kwforall
  sc
  vars <- some alphaNumChar `sepBy` (parseSymbol SymComma >> sc)
  sc
  parseSymbol SymDot
  sc 
  MkTypeScheme vars <$> parseTy

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
