module Parser.Types (
  parseKindedTy,
  parseTy,
  parseTyArgs,
) where 

import Parser.Definition
import Parser.Symbols
import Parser.Keywords
import Parser.Lexer
import Parser.Common
import Syntax.Parsed.Types
import Common

import Text.Megaparsec
import Text.Megaparsec.Char

parseTy :: Parser Ty 
parseTy = parseTyParens <|> parseTyForall <|> parseTyShift <|> parseTyCo <|> try parseTyDecl <|> parseTyvar

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
  args <- parseTypevar `sepBy` space1 
  sc 
  parseSymbol SymDot
  sc 
  TyForall args <$> parseTy 

parseTyDecl :: Parser Ty
parseTyDecl = do
  tyn <- parseTypename 
  parseSymbol SymParensO 
  args <- parseTy `sepBy` (parseSymbol SymComma >> space)
  parseSymbol SymParensC
  return (TyDecl tyn args)

parseTyArgs :: Parser [VariantVar]
parseTyArgs = (do 
  parseSymbol SymParensO
  vars <- parseVariantVar `sepBy` (parseSymbol SymComma >> space)
  parseSymbol SymParensC
  return vars)
  <|>
  return []

parseTyvar :: Parser Ty
parseTyvar = TyVar <$> parseTypevar

parseTyShift :: Parser Ty 
parseTyShift = do 
  parseSymbol SymBrackO
  sc
  ty <- parseTy
  sc
  parseSymbol SymBrackC 
  return (TyShift ty)

parseTyCo :: Parser Ty 
parseTyCo = do 
  parseKeyword KwCo <|> parseKeyword Kwco
  sc
  TyCo <$> parseTy

parseKindedTy :: Parser KindedTy 
parseKindedTy = do 
  ty <- parseTy
  sc 
  parseSymbol SymColon
  sc 
  KindedTy ty <$> parseKind
