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
parseTerm = parseMu <|> parseXCase <|> try parseShiftNeg <|> parseShiftPos <|> try parseXtor <|> parseVar <|> parseParens parseTerm 

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
  nm <- parseXtorname 
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
  nm <- parseXtorname 
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
parseCommand = 
  parseCut <|> 
  parseDone <|> 
  parseErr <|> 
  parsePrint <|>
  parsePrintAnnot <|>
  try parseCutPos <|> 
  try parseCutNeg <|> 
  parseParens parseCommand

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

parseCutAnnot :: Parser (EvaluationOrder,Maybe KindedTy)
parseCutAnnot = try parseEoBarTy <|> try parseTyBarEo <|> (,Nothing) <$> parseEvaluationOrder 

parseEoBarTy :: Parser (EvaluationOrder, Maybe KindedTy)
parseEoBarTy = do
  pol <- parseEvaluationOrder
  sc 
  parseSymbol SymBar
  sc 
  ty <- parseKindedTy 
  notFollowedBy (sc >> parseSymbol SymAngC)
  return (pol,Just ty)

parseTyBarEo :: Parser (EvaluationOrder,Maybe KindedTy)
parseTyBarEo = do 
  ty <- parseKindedTy
  sc 
  parseSymbol SymBar 
  sc 
  pol <- parseEvaluationOrder 
  return (pol,Just ty)


parseCutPos :: Parser Command 
parseCutPos = do 
  startPos <- getCurrPos 
  t <- parseTerm
  sc
  parseSymbol SymAngC
  parseSymbol SymAngC
  sc
  mty <- optional $ try tyAnnot
  u <- parseTerm
  loc <- getCurrLoc startPos 
  case mty of 
    Nothing -> return (Cut loc t CBV u)
    Just ty -> return (CutAnnot loc t ty CBV u)
  where 
    tyAnnot :: Parser KindedTy
    tyAnnot = do 
      ty <- parseKindedTy
      sc
      parseSymbol SymAngC 
      parseSymbol SymAngC
      sc
      return ty

parseCutNeg :: Parser Command 
parseCutNeg = do 
  startPos <- getCurrPos 
  t <- parseTerm 
  sc 
  parseSymbol SymAngO
  parseSymbol SymAngO 
  sc
  mty <- optional $ try tyAnnot
  u <- parseTerm
  loc <- getCurrLoc startPos 
  case mty of 
    Nothing -> return (Cut loc t CBN u)
    Just ty -> return (CutAnnot loc t ty CBN u)
  where 
    tyAnnot :: Parser KindedTy
    tyAnnot = do 
      ty <- parseKindedTy
      sc
      parseSymbol SymAngO
      parseSymbol SymAngO 
      sc 
      return ty

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

parsePrint :: Parser Command 
parsePrint = do 
  startPos <- getCurrPos 
  sc 
  parseKeyword KwPrint <|> parseKeyword Kwprint
  sc 
  t <- parseTerm
  sc 
  loc <- getCurrLoc startPos
  return (Print loc t)

parsePrintAnnot :: Parser Command 
parsePrintAnnot = do
  startPos <- getCurrPos
  sc 
  parseKeyword KwPrint <|> parseKeyword Kwprint 
  sc 
  t <- parseTerm 
  sc 
  parseSymbol SymColon
  parseSymbol SymColon
  sc 
  ty <- parseKindedTy
  loc <- getCurrLoc startPos
  return (PrintAnnot loc t ty)

