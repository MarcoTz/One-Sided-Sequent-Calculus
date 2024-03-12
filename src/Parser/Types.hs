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
parseTy = parseTyParens <|> parseTyForall <|> parseTyCo <|> try parseTyDecl <|> parseTyVar

parseTyParens :: Parser Ty
parseTyParens = do 
  parseSymbol SymParensO
  sc
  ty <- parseTy
  sc
  parseSymbol SymParensC
  return ty

parseTyForall :: Parser Ty 
parseTyForall = do
  parseKeyword KwForall <|> parseKeyword Kwforall 
  sc 
  args <- parseTypeVar `sepBy` space1 
  sc 
  parseSymbol SymDot
  sc 
  TyForall args <$> parseTy 

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

parsePolTy :: Parser PolTy
parsePolTy = do 
  ty <- parseTy
  sc 
  parseSymbol SymColon
  sc 
  pol <- parsePol
  return (ty, pol)
