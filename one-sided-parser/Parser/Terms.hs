module Parser.Terms (
  parseTerm,
  parseCommand
) where

import Parser.Definition
import Parser.Lexer
import Parser.Symbols
import Parser.Keywords
import Parser.Types
import Syntax.Parsed.Terms
import Syntax.Parsed.Types
import Common

import Text.Megaparsec
import Data.Functor

parseTerm :: Parser Term
parseTerm = parseMu <|> parseXCase <|> try parseShiftNeg <|> parseShiftPos <|> try parseXtor <|> parseVar

parseVar :: Parser Term 
parseVar = Var <$> parseVariable 

parseMu :: Parser Term
parseMu = do
  parseKeyword KwMu <|> parseKeyword Kwmu 
  sc
  v <- parseVariable 
  sc
  parseSymbol SymDot
  sc
  Mu v <$> parseCommand

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

parsePattern :: Parser Pattern 
parsePattern = do 
  nm <- parseXtorName 
  sc
  args <- parseParens (parseVariable `sepBy` parseCommaSep) <|> sc $> []
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

parseShiftPos :: Parser Term 
parseShiftPos = do 
  parseSymbol SymBrackO
  t <- parseTerm
  parseSymbol SymBrackC
  return (ShiftPos t)

parseShiftNeg :: Parser Term
parseShiftNeg = do 
  parseSymbol SymBrackO
  sc
  v <- parseVariable 
  sc
  parseSymbol SymBrackC
  sc
  parseSymbol SymDot
  sc
  ShiftNeg v <$> parseCommand

parseCommand :: Parser Command 
parseCommand = parseCut <|> parseDone <|> parseErr

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

parseCutAnnot :: Parser (Pol,Maybe PolTy)
parseCutAnnot = try parsePolBarTy <|> parseTyBarPol <|> (,Nothing) <$> parsePol 

parsePolBarTy :: Parser (Pol, Maybe PolTy)
parsePolBarTy = do
  pol <- parsePol 
  sc 
  parseSymbol SymBar
  sc 
  ty <- parsePolTy 
  notFollowedBy (sc >> parseSymbol SymAngC)
  return (pol,Just ty)

parseTyBarPol :: Parser (Pol,Maybe PolTy)
parseTyBarPol = do 
  ty <- parsePolTy
  sc 
  parseSymbol SymBar 
  sc 
  pol <- parsePol 
  return (pol,Just ty)

parseDone :: Parser Command
parseDone = parseKeyword KwDone  >> return Done

parseErr :: Parser Command 
parseErr = do 
  parseKeyword KwError
  sc 
  parseSymbol SymQuot
  msg <- takeWhileP (Just "character") (/= '"')
  parseSymbol SymQuot
  sc
  return (Err msg)
