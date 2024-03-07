module Parser.Terms where

import Parser.Definition
import Parser.Lexer
import Parser.Symbols
import Parser.Keywords
import Parser.Types
import Syntax.Parsed.Terms
import Syntax.Parsed.Types
import Common

import Text.Megaparsec

import Pretty.Terms() 

parseTerm :: Parser Term
parseTerm = parseMu <|> parseXCase <|> parseShift <|> parseLam <|> try parseXtor <|> parseVar

parseVar :: Parser Term 
parseVar = Var <$> parseVariable 

parseMu :: Parser Term
parseMu = do
  parseKeyword KwMu <|> parseKeyword Kwmu 
  sc
  v <- parseVariable 
  sc
  mpol <- optional (do 
    parseSymbol SymColon
    pol <- parsePol 
    sc
    return pol)
  parseSymbol SymDot
  sc
  Mu v mpol <$> parseCommand

parseXtor :: Parser Term
parseXtor = do
  nm <- parseXtorName 
  sc
  parseSymbol SymParensO
  sc
  args <- parseTerm `sepBy` (parseSymbol SymComma >> sc)
  sc
  parseSymbol SymParensC 
  return $ Xtor nm args

parsePatternVars :: Parser [Variable]
parsePatternVars = (do 
  parseSymbol SymParensO
  args <- parseVariable `sepBy` (parseSymbol SymComma >> sc)
  parseSymbol SymParensC 
  return args)
  <|>
  return []

parsePattern :: Parser Pattern 
parsePattern = do 
  nm <- parseXtorName 
  sc
  args <- parsePatternVars 
  sc
  parseSymbol SymEq 
  parseSymbol SymAngC
  sc
  MkPattern nm args <$> parseCommand

parseXCase :: Parser Term
parseXCase = do 
  parseKeyword KwCase
  sc
  parseSymbol SymBrackO
  sc
  pts <- parsePattern `sepBy` (parseSymbol SymComma >> sc)
  sc
  parseSymbol SymBrackC
  sc
  return (XCase pts)

parseShift :: Parser Term 
parseShift = do 
  parseSymbol SymBrackO
  t <- parseTerm
  parseSymbol SymBrackC
  return (Shift t)

parseLam :: Parser Term
parseLam = do 
  parseKeyword KwLam
  sc
  parseSymbol SymBrackO
  sc
  v <- parseVariable 
  sc
  parseSymbol SymBrackC
  sc
  parseSymbol SymDot
  sc
  Lam v <$> parseCommand


parseCommand :: Parser Command 
parseCommand = parseCut <|> parseDone

parseCut :: Parser Command
parseCut = do 
  parseSymbol SymAngO
  sc
  t <- parseTerm
  sc
  parseSymbol SymBar
  sc
  (pol,mty) <- parseCutAnnot 
  sc
  parseSymbol SymBar
  sc
  u <- parseTerm
  sc
  parseSymbol SymAngC
  case mty of 
    Nothing -> return (Cut t pol u)
    Just ty -> return (CutAnnot t ty pol u)

parseCutAnnot :: Parser (Pol,Maybe Ty)
parseCutAnnot = try parsePolTy <|> parseTyPol <|> (,Nothing) <$> parsePol 

parsePolTy :: Parser (Pol, Maybe Ty)
parsePolTy = do
  pol <- parsePol 
  sc 
  parseSymbol SymBar
  sc 
  ty <- parseTy 
  notFollowedBy (sc >> parseSymbol SymAngC)
  return (pol,Just ty)

parseTyPol :: Parser (Pol,Maybe Ty)
parseTyPol = do 
  ty <- parseTy 
  sc 
  parseSymbol SymBar 
  sc 
  pol <- parsePol 
  return (pol,Just ty)

parseDone :: Parser Command
parseDone = parseKeyword KwDone  >> return Done

