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
parseTy = parseTyParens <|> parseTyCo <|> try parseTyDecl <|> parseTyVar

parseTyParens :: Parser Ty
parseTyParens = do 
  parseSymbol SymParensO
  sc
  ty <- parseTy
  sc
  parseSymbol SymParensC
  return ty

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

parseTyArgs :: Parser [PolVar]
parseTyArgs = (do 
  parseSymbol SymParensO
  vars <- parsePolVar `sepBy` (parseSymbol SymComma >> space)
  parseSymbol SymParensC
  return vars)
  <|>
  return []

parseTyVar :: Parser Ty
parseTyVar = TyVar <$> parseTypeVar

parseTyCo :: Parser Ty 
parseTyCo = do 
  parseKeyword KwCo <|> parseKeyword Kwco
  sc
  TyCo <$> parseTy


parseVariableTy :: Parser (Variable, Maybe Ty)
parseVariableTy = try (do 
  v <- parseVariable
  sc 
  parseSymbol SymColon
  sc 
  ty <- parseTy
  return (v, Just ty)) 
  <|> (,Nothing) <$> parseVariable
