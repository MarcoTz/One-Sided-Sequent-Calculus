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


parsePol :: Parser Pol 
parsePol = (parseSymbol SymPlus >> return Pos) <|> (parseSymbol SymMinus >> return Neg)
