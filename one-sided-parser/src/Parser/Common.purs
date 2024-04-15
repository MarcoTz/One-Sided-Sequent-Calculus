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

import Common (EvaluationOrder(..),Variance(..),Kind(..),Modulename(..),Variable(..),Typename(..),Typevar(..), DeclTy(..), Kindvar(..), Xtorname(..), VariantVar(..))
import Parser.Definition (SrcParser)
import Parser.Lexer (sc, parseKeyword, parseSymbol, parseIdentifier)
import Parser.Keywords (Keyword(..))
import Parser.Symbols (Sym(..))

import Prelude (bind,pure,(<$>), ($))
import Control.Alt ((<|>))

parseEvaluationOrder :: SrcParser EvaluationOrder 
parseEvaluationOrder = 
  (do
  _ <- parseKeyword KwCBV
  pure CBV) 
  <|> 
  (do
  _ <- parseKeyword KwCBN 
  pure CBN)

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

parseKind :: SrcParser Kind 
parseKind = 
  (MkKind <$> parseEvaluationOrder) 
  <|> 
  (do
  nm <- parseIdentifier
  pure $ MkKindVar (Kindvar {unKindvar:nm}))

parseModulename :: SrcParser Modulename
parseModulename = do
  nm <- parseIdentifier 
  pure $ Modulename {unModulename : nm} 

parseVariable :: SrcParser Variable
parseVariable = do 
  nm <- parseIdentifier
  pure $ Variable {unVariable:nm}

parseXtorname :: SrcParser Xtorname 
parseXtorname = do 
  nm <- parseIdentifier
  pure $ Xtorname { unXtorname:nm}

parseTypename :: SrcParser Typename
parseTypename = do
  nm <- parseIdentifier 
  pure $ Typename {unTypename:nm}

parseTypevar :: SrcParser Typevar
parseTypevar = do 
  nm <- parseIdentifier 
  pure $ Typevar {unTypevar:nm}

parseVariantVar :: SrcParser VariantVar 
parseVariantVar = do 
  tyv <- parseTypevar 
  _ <- sc 
  _ <- parseSymbol SymColon
  _ <- sc 
  var <- parseVariance
  pure $ VariantVar {variantVar:tyv, variantVariance:var }
