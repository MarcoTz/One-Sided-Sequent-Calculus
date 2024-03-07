module Parser.Lexer where 

import Parser.Keywords
import Parser.Symbols
import Parser.Definition
import Common
 
import Text.Megaparsec 
import Text.Megaparsec.Char
import Control.Monad
import Data.Text qualified as T

parseKeyword :: Keyword -> Parser () 
parseKeyword kw = do
 _ <- string (T.pack (show kw))
 return () 

parseSymbol :: Sym -> Parser () 
parseSymbol sym = do 
  _ <- string (T.pack (show sym))
  return ()

parseComment :: Parser()
parseComment = do
  space
  parseSymbol SymMinus
  parseSymbol SymMinus
  _ <- manyTill anySingle eol
  return ()

sc :: Parser () 
sc = try parseComment <|> space

parsePol :: Parser Pol 
parsePol = (parseSymbol SymPlus >> return Pos) <|> (parseSymbol SymMinus >> return Neg)

parseIdentifier :: Parser String
parseIdentifier = do
  ident <- some alphaNumChar
  guard (ident `notElem` (show <$> allKws))
  return ident
--  then throwError $ ErrParser ("Identifier " <> ident <> " is reserved") 
--  else return ident

parseModulename :: Parser Modulename
parseModulename = MkModule <$> parseIdentifier

parseVariable :: Parser Variable
parseVariable = MkVar <$>  parseIdentifier

parseXtorName :: Parser XtorName 
parseXtorName = MkXtorName <$> parseIdentifier 

parseTypeName :: Parser TypeName
parseTypeName = MkTypeName <$> parseIdentifier 

parseTypeVar :: Parser TypeVar
parseTypeVar = MkTypeVar <$> parseIdentifier 

parsePolVar :: Parser PolVar 
parsePolVar = do
  tyv <- parseTypeVar 
  sc 
  parseSymbol SymColon
  sc 
  MkPolVar tyv <$> parsePol
