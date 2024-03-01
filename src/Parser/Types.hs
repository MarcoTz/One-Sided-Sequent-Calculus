module Parser.Types where 

import Parser.Definition
import Parser.Symbols
import Parser.Keywords
import Parser.Lexer
import Syntax.Parsed.Program
import Common

import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace 
import Pretty.Types ()

parseTy :: Parser Ty 
parseTy = try parseTyDecl <|> parseTyVar

parseTypeScheme :: Parser TypeScheme 
parseTypeScheme = do 
  parseKeyword KwForall <|> parseKeyword Kwforall
  sc
  vars <- some alphaNumChar `sepBy` (parseSymbol SymComma >> sc)
  trace ("parsed forall " <> show vars ) $ return ()
  sc
  parseSymbol SymDot
  sc 
  ty <- parseTy
  return $ MkTypeScheme vars ty

parseTyDecl :: Parser Ty
parseTyDecl = do
  tyn <- some alphaNumChar
  parseSymbol SymParensO 
  args <- parseTy `sepBy` (parseSymbol SymComma >> space)
  parseSymbol SymParensC
  trace ("parsed declared type " <> tyn <> "( " <> show args <> ")") $ return ()
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
