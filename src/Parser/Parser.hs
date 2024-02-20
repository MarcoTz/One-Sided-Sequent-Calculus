module Parser.Parser where 

import Parser.Keywords
import Parser.Symbols
import Untyped.Program
import Common

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Plus
import Data.Text qualified as T
import Control.Applicative (Alternative)

import Pretty ()

newtype Parser a = Parser { getParser :: Parsec String T.Text a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec String T.Text)

runFileParser :: FilePath -> Parser b -> T.Text -> Either (ParseErrorBundle T.Text String) b
runFileParser fp p input = case runParser (getParser p) fp input of 
  Left s -> Left s
  Right x -> pure x 

parseKeyword :: Keyword -> Parser () 
parseKeyword kw = do
 _ <- string (T.pack (show kw))
 return () 

parseSymbol :: Sym -> Parser () 
parseSymbol sym = do 
  _ <- string (T.pack (show sym))
  return ()


parsePol :: Parser Pol 
parsePol = (parseSymbol SymPlus >> return Pos) <|> (parseSymbol SymMinus >> return Neg)

parsePolVar :: Parser (Variable,Pol)
parsePolVar = do 
  nm <- some alphaNumChar
  parseSymbol SymColon
  pol <- parsePol
  return (nm,pol)

parseXtorSig :: Parser XtorSig
parseXtorSig = do 
 nm <- some alphaNumChar
 MkXtorSig nm <$> parseXtorArgs
 
parseXtorArgs :: Parser [Ty]
parseXtorArgs = (do 
  parseSymbol SymParensO
  vars <- parseTy `sepBy` (parseSymbol SymComma >> optional space1) 
  parseSymbol SymParensC
  return vars)
  <|>
  return []

parseTy :: Parser Ty 
parseTy = try parseTyDecl <|> parseTyVar

parseTyDecl :: Parser Ty
parseTyDecl = do
  tyn <- some alphaNumChar
  parseSymbol SymParensO 
  args <- parseTy `sepBy` (parseSymbol SymComma >> optional space1)
  parseSymbol SymParensC
  return (TyDecl tyn args)

parseTyVar :: Parser Ty
parseTyVar = do 
    var <- some alphaNumChar
    return (TyVar var)


parseTyArgs :: Parser [(Variable, Pol)]
parseTyArgs = (do 
  parseSymbol SymParensO
  vars <- parsePolVar `sepBy` (parseSymbol SymComma >> optional space1)
  parseSymbol SymParensC
  return vars)
  <|>
  return []

parseopteol :: Parser () 
parseopteol = do 
 _ <- optional eol
 return ()

  
parseDecl :: Parser DataDecl 
parseDecl = do 
  parseKeyword KwData
  space1
  nm <- some alphaNumChar
  args <- parseTyArgs
  space1
  parseSymbol SymColon
  space1
  pol <- parsePol
  space1
  _ <- optional eol
  parseSymbol SymBrackO
  _ <- optional eol
  space1 
  xtors <- parseXtorSig `sepBy` (parseSymbol SymComma >> optional space1)
  space1
  _ <- optional eol
  parseSymbol SymBrackC
  return MkDataDecl{declNm=nm, declArgs=args,declPol=pol, declSig=xtors}

parseProgram :: Parser [DataDecl]
parseProgram = some parseDecl 

