module Parser.Lexer where 

import Parser.Keywords
import Parser.Symbols
import Parser.Definition
import Common
 
import Text.Megaparsec 
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Monad
import Data.Text qualified as T


parseParens :: Parser a -> Parser a
parseParens p = do 
  parseSymbol SymParensO 
  sc
  a <- p
  sc
  parseSymbol SymParensC 
  return a

parseKeyword :: Keyword -> Parser () 
parseKeyword kw = do
 _ <- string (T.pack (show kw))
 return () 

parseSymbol :: Sym -> Parser () 
parseSymbol sym = do 
  _ <- string (T.pack (show sym))
  return ()

parseComment :: Parser()
parseComment = try $ do
  parseSymbol SymMinus
  parseSymbol SymMinus
  _ <- takeWhileP (Just "character") (/= '\n')
  return ()

sc :: Parser () 
sc = L.space space1 parseComment empty 

parsePol :: Parser Pol 
parsePol = (parseSymbol SymPlus >> return Pos) <|> (parseSymbol SymMinus >> return Neg)

parseCommaSep :: Parser ()
parseCommaSep = parseSymbol SymComma>>sc

parseIdentifier :: Parser String
parseIdentifier = do
  ident <- some alphaNumChar
  guard (ident `notElem` (show <$> allKws))
  return ident

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
