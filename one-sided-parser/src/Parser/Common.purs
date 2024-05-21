module Parser.Common (
  parseTypevar,
  parseTypename,
  parseVariantVar,
  parseEvaluationOrder,
  parseVariable,
  parseXtorname,
  parseModulename,
  parseDataCodata
) where 

import Common (
  EvaluationOrder(..),Variance(..),  Modulename(..),Variable(..),
  Typename(..),Typevar(..), DeclTy(..), Xtorname(..), VariantVar(..))
import Parser.Definition (SrcParser)
import Parser.Lexer (sc, parseKeyword, parseSymbol, parseIdentifier)
import Parser.Keywords (Keyword(..))
import Parser.Symbols (Sym(..))

import Prelude (bind,pure,($),(*>))
import Control.Alt ((<|>))

parseEvaluationOrder :: SrcParser EvaluationOrder 
parseEvaluationOrder = 
  parseKeyword KwCBV *> pure CBV
  <|>
  parseKeyword KwCBN *> pure CBN
  <|>
  parseKeyword KwAny *> pure Any

parseDataCodata :: SrcParser DeclTy
parseDataCodata = 
  (do
  _ <- parseKeyword KwData
  pure Data) 
  <|> 
  (do
  _ <- parseKeyword KwCodata
  pure Codata)

parseVariance :: SrcParser Variance 
parseVariance = 
 (do
 _ <- parseSymbol SymPlus
 pure Covariant) 
 <|>
 (do 
 _ <- parseSymbol SymMinus 
 pure Contravariant)


parseModulename :: SrcParser Modulename
parseModulename = do
  nm <- parseIdentifier 
  pure $ Modulename nm 

parseVariable :: SrcParser Variable
parseVariable = do 
  nm <- parseIdentifier
  pure $ Variable nm 

parseXtorname :: SrcParser Xtorname 
parseXtorname = do 
  nm <- parseIdentifier
  pure $ Xtorname nm

parseTypename :: SrcParser Typename
parseTypename = do
  nm <- parseIdentifier 
  pure $ Typename nm 

parseTypevar :: SrcParser Typevar
parseTypevar = do 
  nm <- parseIdentifier 
  pure $ Typevar nm 

parseVariantVar :: SrcParser VariantVar 
parseVariantVar = do 
  tyv <- parseTypevar 
  _ <- sc 
  _ <- parseSymbol SymColon
  _ <- sc 
  var <- parseVariance
  pure $ VariantVar {variantVar:tyv, variantVariance:var }
