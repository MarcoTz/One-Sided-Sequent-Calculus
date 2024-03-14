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


parseTerm :: Parser Term
parseTerm = parseMu <|> parseXCase <|> parseShiftPos <|> parseShiftNeg <|> try parseXtor <|> parseVar

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

parseMTypedVar :: Parser MTypedVar 
parseMTypedVar = try (do 
  var <- parseVariable 
  sc 
  parseSymbol SymColon
  sc 
  pty <- parsePolTy
  return (var, Just pty)) 
  <|>
  (,Nothing) <$> parseVariable


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
