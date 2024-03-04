module Parser.Types where 

import Parser.Definition
import Parser.Symbols
import Parser.Lexer
import Syntax.Parsed.Types
import Common

import Text.Megaparsec
import Text.Megaparsec.Char


parseTy :: Parser Ty 
parseTy = try parseTyDecl <|> parseTyVar 

--parseTyForall :: Parser Ty
--parseTyForall = do 
--  parseKeyword KwForall <|> parseKeyword Kwforall
--  sc
--  vars <- parsePolVar `sepBy` (parseSymbol SymComma >> sc)
--  sc
--  parseSymbol SymDot
--  sc 
--  TyForall vars <$> parseTy

parseTyDecl :: Parser Ty
parseTyDecl = do
  tyn <- parseTypeName 
  parseSymbol SymParensO 
  args <- parseTy `sepBy` (parseSymbol SymComma >> space)
  parseSymbol SymParensC
  return (TyDecl tyn args)

parseTyVar :: Parser Ty
parseTyVar = TyVar <$> parseTypeVar

parseTyArgs :: Parser [PolVar]
parseTyArgs = (do 
  parseSymbol SymParensO
  vars <- parsePolVar `sepBy` (parseSymbol SymComma >> space)
  parseSymbol SymParensC
  return vars)
  <|>
  return []
