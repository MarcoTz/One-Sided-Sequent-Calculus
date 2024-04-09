module Parser.Common (
  parseTypevar,
  parseTypename,
  parseVariantVar,
  parseEvaluationOrder,
  parseKind,
  parseVariable,
  parseXtorname,
  parseModulename,
  parseDataCodata
) where 

import Parser.Definition
import Parser.Symbols
import Parser.Keywords
import Parser.Lexer
import Common

import Text.Megaparsec

parseEvaluationOrder :: Parser EvaluationOrder 
parseEvaluationOrder = (parseKeyword KwCBV >> return CBV) <|> (parseKeyword KwCBN >> return CBN)

parseDataCodata :: Parser DeclTy
parseDataCodata = 
  (parseKeyword KwData >> return Data) <|> 
  (parseKeyword KwCodata >> return Codata)

parseVariance :: Parser Variance 
parseVariance = 
 (parseSymbol SymPlus >> return Covariant) <|>
 (parseSymbol SymMinus >> return Contravariant)

parseKind :: Parser Kind 
parseKind = 
  (MkKind <$> parseEvaluationOrder) <|> 
  (MkKindVar  . Kindvar <$> parseIdentifier)

parseModulename :: Parser Modulename
parseModulename = Modulename <$> parseIdentifier

parseVariable :: Parser Variable
parseVariable = Variable <$>  parseIdentifier

parseXtorname :: Parser Xtorname 
parseXtorname = Xtorname <$> parseIdentifier 

parseTypename :: Parser Typename
parseTypename = Typename <$> parseIdentifier 

parseTypevar :: Parser Typevar
parseTypevar = Typevar <$> parseIdentifier 

parseVariantVar :: Parser VariantVar 
parseVariantVar = do 
  tyv <- parseTypevar 
  sc 
  parseSymbol SymColon
  sc 
  VariantVar tyv <$> parseVariance
