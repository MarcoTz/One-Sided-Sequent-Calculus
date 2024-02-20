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

import Debug.Trace
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
 trace ("parsed keyword " <> show kw) $ return ()
 return () 

parseSymbol :: Sym -> Parser () 
parseSymbol sym = do 
  _ <- string (T.pack (show sym))
  trace ("parsed symbol " <> show sym) $ return ()
  return ()


parsePol :: Parser Pol 
parsePol = do 
  pl <- (parseSymbol SymPlus >> return Pos) <|> (parseSymbol SymMinus >> return Neg)
  trace ("parsed polarity " <> show pl) $ return ()
  return pl

parsePolVar :: Parser (Variable,Pol)
parsePolVar = do 
  nm <- some alphaNumChar
  parseSymbol SymColon
  pol <- parsePol
  trace ("parsed polvar " <> show nm <> ":" <> show pol) $ return () 
  return (nm,pol)

parseXtorSig :: Parser XtorSig
parseXtorSig = do 
 trace "parsing xtorsig" $ return ()
 nm <- some alphaNumChar
 trace ("parsed name " <> nm) $ return ()
 args <- parseXtorArgs
 trace ("parsed args " <> show args) $ return () 
 return (MkXtorSig nm args)
 
parseXtorArgs :: Parser [Ty]
parseXtorArgs = (do 
  parseSymbol SymParensO
  vars <- parseTy `sepBy` (parseSymbol SymComma >> optional space1) 
  trace ("parsed args" <> show vars ) $ return () 
  parseSymbol SymParensC
  return vars)
  <|>
  return []

parseTy :: Parser Ty 
parseTy = try parseTyDecl <|> parseTyVar

parseTyDecl :: Parser Ty
parseTyDecl = do
  trace "parsing decl type" $ return ()
  tyn <- some alphaNumChar
  trace ("parsed name " <> tyn) $ return ()
  parseSymbol SymParensO 
  args <- parseTy `sepBy` (parseSymbol SymComma >> optional space1)
  parseSymbol SymParensC
  return (TyDecl tyn args)

parseTyVar :: Parser Ty
parseTyVar = do 
    var <- some alphaNumChar
    trace ("parsed var" <> var) $ return (TyVar var)


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
  trace "parsing declaration " $ return () 
  nm <- some alphaNumChar
  trace ("parsed name " <> nm) $ return () 
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
  trace "parsed xtors" $ return ()
  space1
  _ <- optional eol
  parseSymbol SymBrackC
  return MkDataDecl{declNm=nm, declArgs=args,declPol=pol, declSig=xtors}

parseProgram :: Parser [DataDecl]
parseProgram = some parseDecl 

