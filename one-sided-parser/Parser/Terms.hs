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
parseVar = do 
  startPos <- getCurrPos
  v <- parseVariable
  loc <- getCurrLoc startPos
  return $ Var loc v

parseMu :: Parser Term
parseMu = do
  startPos <- getCurrPos
  parseKeyword KwMu <|> parseKeyword Kwmu 
  sc
  v <- parseVariable 
  sc
  parseSymbol SymDot
  sc
  c <- parseCommand
  loc <- getCurrLoc startPos
  return $ Mu loc v c 

parseXtor :: Parser Term
parseXtor = do
  startPos <- getCurrPos 
  nm <- parseXtorName 
  sc
  parseSymbol SymParensO
  sc
  args <- parseTerm `sepBy` (parseSymbol SymComma >> sc)
  sc
  parseSymbol SymParensC 
  loc <- getCurrLoc startPos
  return $ Xtor loc nm args

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
  startPos <- getCurrPos
  parseKeyword KwCase
  sc
  parseSymbol SymBrackO
  sc
  pts <- parsePattern `sepBy` (parseSymbol SymComma >> sc)
  sc
  parseSymbol SymBrackC
  loc <- getCurrLoc startPos
  return (XCase loc pts)

parseShiftPos :: Parser Term 
parseShiftPos = do 
  startPos <- getCurrPos
  parseSymbol SymBrackO
  t <- parseTerm
  loc <- getCurrLoc startPos
  parseSymbol SymBrackC
  return (ShiftPos loc t)

parseShiftNeg :: Parser Term
parseShiftNeg = do 
  startPos <- getCurrPos
  parseSymbol SymBrackO
  sc
  v <- parseVariable 
  sc
  parseSymbol SymBrackC
  sc
  parseSymbol SymDot
  sc
  c <- parseCommand
  loc <- getCurrLoc startPos
  return $ ShiftNeg loc v c

parseCommand :: Parser Command 
parseCommand = parseCut <|> parseDone <|> parseErr

parseCut :: Parser Command
parseCut = do 
  startPos <- getCurrPos
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
  loc <- getCurrLoc startPos 
  case mty of 
    Nothing -> return (Cut loc t pol u)
    Just ty -> return (CutAnnot loc t ty pol u)

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
parseDone = do
  startPos <- getCurrPos
  parseKeyword KwDone  
  loc <- getCurrLoc startPos 
  return (Done loc)

parseErr :: Parser Command 
parseErr = do 
  startPos <- getCurrPos
  parseKeyword KwError
  sc 
  parseSymbol SymQuot
  msg <- takeWhileP (Just "character") (/= '"')
  parseSymbol SymQuot
  loc <- getCurrLoc startPos
  return (Err loc msg)
