module Parser.Lexer (
  sc,
  parseSymbol,
  parseKeyword,
  parsePol,
  parseTypevar,
  parsePolvar,
  parseTypename,
  parseXtorname,
  parseModulename,
  parseVariable,
  parseCommaSep,
  parseParens,
  getCurrPos,
  getCurrLoc
) where 

import Parser.Keywords
import Parser.Symbols
import Parser.Definition
import Common
import Loc
 
import Text.Megaparsec 
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Monad


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
 _ <- string (show kw)
 return () 

parseSymbol :: Sym -> Parser () 
parseSymbol sym = do 
  _ <- string (show sym)
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
parseModulename = Modulename <$> parseIdentifier

parseVariable :: Parser Variable
parseVariable = Variable <$>  parseIdentifier

parseXtorname :: Parser Xtorname 
parseXtorname = Xtorname <$> parseIdentifier 

parseTypename :: Parser Typename
parseTypename = Typename <$> parseIdentifier 

parseTypevar :: Parser Typevar
parseTypevar = Typevar <$> parseIdentifier 

parsePolvar :: Parser Polvar 
parsePolvar = do
  tyv <- parseTypevar 
  sc 
  parseSymbol SymColon
  sc 
  Polvar tyv <$> parsePol


getCurrPos :: Parser SourcePosition
getCurrPos = do 
  SourcePos _ line column <- getSourcePos
  return (MkSourcePos (unPos line) (unPos column))

getCurrLoc :: SourcePosition -> Parser Loc 
getCurrLoc startPos = MkLoc startPos <$> getCurrPos 
