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

newtype Parser a = Parser { getParser :: Parsec String T.Text a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadParsec String T.Text)

runFileParser :: FilePath -> Parser b -> T.Text -> Either (ParseErrorBundle T.Text String) b
runFileParser fp p input = case runParser (getParser p) fp input of 
  Left s -> Left s
  Right x -> pure x 

parseKeyWord :: Keyword -> Parser () 
parseKeyWord kw = do
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
 parseSymbol SymParensO
 args <- some alphaNumChar `sepBy` parseSymbol SymComma
 parseSymbol SymParensC
 return (MkXtorSig nm args)
  
parseDecl :: Parser DataDecl 
parseDecl = do 
  nm <- some alphaNumChar
  parseSymbol SymParensO
  argVars <- parsePolVar `sepBy` parseSymbol SymComma
  parseSymbol SymParensC
  parseSymbol SymColon
  pol <- parsePol
  parseKeyWord KwWhere 
  parseSymbol SymBrackO
  xtors <- some parseXtorSig
  parseSymbol SymBrackC
  return MkDataDecl{declNm=nm, declArgs=argVars,declPol=pol, declSig=xtors}

parseProgram :: Parser [DataDecl]
parseProgram = parseDecl `sepBy` newline 
