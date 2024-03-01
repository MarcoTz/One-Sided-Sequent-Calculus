module Parser.Lexer where 

import Parser.Keywords
import Parser.Symbols
import Parser.Definition
import Common

import Text.Megaparsec 
import Text.Megaparsec.Char
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

parseVariable :: Parser Variable
parseVariable = MkVar <$> some alphaNumChar

parseXtorName :: Parser XtorName 
parseXtorName = MkXtorName <$> some alphaNumChar

parseTypeName :: Parser TypeName
parseTypeName = MkTypeName <$> some alphaNumChar

parseTypeVar :: Parser TypeVar
parseTypeVar = MkTypeVar <$> some alphaNumChar
