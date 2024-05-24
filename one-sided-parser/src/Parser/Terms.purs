module Parser.Terms (
  parseTerm,
  parseCommand
) where

import Parser.Definition (SrcParser)
import Parser.Lexer (sc, sc1, getCurrPos,getCurrLoc, parseParens,parseKeyword, parseSymbol, parseCommaSep, parseAngC, parseAngO)
import Parser.Common (parseVariable, parseXtorname, parseEvaluationOrder)
import Parser.Keywords (Keyword(..))
import Parser.Symbols (Sym(..))
import Parser.Types (parseTy)
import Loc (getLoc)
import Common (EvaluationOrder(..))
import Syntax.Parsed.Terms (Term(..),Command(..),Pattern(..))
import Syntax.Parsed.Types (Ty)

import Prelude (bind, ($),pure, (<>))
import Data.List (List(..)) 
import Data.List.NonEmpty (toList) 
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Unit (unit)
import Data.String.CodeUnits (singleton)
import Parsing (fail)
import Parsing.Combinators (try, sepBy,sepBy1, many, optionMaybe)
import Parsing.String.Basic (noneOf)
import Control.Alt ((<|>))


parseTerm :: SrcParser Term 
parseTerm = do 
  t1 <- (parseT <|> parseParens parseT)
  _ <- sc
  t1' <- try (parseSeq t1 <|> parseAnd t1 <|> parseOr t1 <|> parseApp t1) <|> pure t1
  pure t1'
  where 
    appTerm :: Term -> List Term -> Term 
    appTerm t1 Nil = t1 
    appTerm t1 (Cons t2 ts) = appTerm (App (getLoc t1) t1 t2) ts

parseApp :: Term -> SrcParser Term
parseApp t1 = do 
  ts <- parseTerm `sepBy1` parseCommaSep
  pure $ appTerm t1 (toList ts)
  where 
    appTerm :: Term -> List Term -> Term
    appTerm t Nil = t
    appTerm t (Cons t2 ts) = appTerm (App (getLoc t2) t t2) ts

parseSeq :: Term -> SrcParser Term
parseSeq t1 = do
  startPos <- getCurrPos 
  _ <- parseSymbol SymSemi
  _ <- parseSymbol SymSemi
  _ <- sc 
  t2 <- parseTerm
  loc <- getCurrLoc startPos
  pure $ Seq loc t1 t2

parseAnd :: Term -> SrcParser Term 
parseAnd t1 = do
  startPos <- getCurrPos
  _ <- parseSymbol SymAmper
  _ <- parseSymbol SymAmper
  _ <- sc 
  t2 <- parseTerm
  loc <- getCurrLoc startPos
  pure (AndBool loc t1 t2)

parseOr :: Term -> SrcParser Term
parseOr t1 = do 
  _ <- sc
  startPos <- getCurrPos 
  _ <- parseSymbol SymBar
  _ <- parseSymbol SymBar
  _ <- sc
  t2 <- parseTerm
  loc <- getCurrLoc startPos
  pure (OrBool loc t1 t2)

parseT :: SrcParser Term
parseT =
  (\_ -> parseMu)       unit <|> 
  (\_ -> parseXCase)    unit <|>
  (\_ -> parseShift)    unit <|>
  (\_ -> parseLam)      unit <|>
  (\_ -> parseLst)      unit <|>
  (\_ -> parseNot)      unit <|>
  (\_ -> parseIf)       unit <|>
  (\_ -> try parseTup)  unit <|>
  (\_ -> try parseXtor) unit <|> 
  (\_ -> parseVar)      unit 

parseIf :: SrcParser Term
parseIf = do 
  startPos <- getCurrPos 
  _ <- parseKeyword KwIf <|> parseKeyword Kwif
  _ <- sc1
  b <- parseTerm 
  _ <- sc1
  _ <- parseKeyword KwThen <|> parseKeyword Kwthen
  _ <- sc1
  t1 <- parseTerm
  _ <- sc1
  _ <- parseKeyword KwElse <|> parseKeyword Kwelse
  _ <- sc1
  t2 <- parseTerm 
  loc <- getCurrLoc startPos 
  pure $ IfThenElse loc b t1 t2

parseNot :: SrcParser Term
parseNot = do 
  startPos <- getCurrPos 
  _ <- parseSymbol SymExcl
  t <- parseTerm
  loc <- getCurrLoc startPos
  pure $ NotBool loc t

parseTup :: SrcParser Term 
parseTup = do 
  startPos <- getCurrPos 
  _ <- parseSymbol SymParensO 
  _ <- sc
  t1 <- parseTerm
  _ <- sc 
  _ <- parseSymbol SymComma
  _ <- sc
  ts <- parseTerm `sepBy` parseCommaSep 
  _ <- sc
  _ <- parseSymbol SymParensC
  loc <- getCurrLoc startPos
  pure $ Tup loc (Cons t1 ts)

parseLst :: SrcParser Term
parseLst = do 
  startPos <- getCurrPos 
  _ <- parseSymbol SymSqBrackO 
  _ <- sc
  ts <- parseTerm `sepBy` parseCommaSep 
  _ <- sc 
  _ <- parseSymbol SymSqBrackC 
  loc <- getCurrLoc startPos
  pure (Lst loc ts)

parseMu :: SrcParser Term   
parseMu = do 
  startPos <- getCurrPos
  _ <- parseKeyword KwMu <|> parseKeyword Kwmu  <|> parseSymbol SymMu
  _ <- sc
  v <- parseVariable 
  _ <- sc
  _ <- parseSymbol SymDot
  _ <- sc
  c <- parseCommand
  loc <- getCurrLoc startPos
  pure $ Mu loc v c

parseXCase :: SrcParser Term 
parseXCase = do
  startPos <- getCurrPos
  _ <- parseKeyword KwCase <|> parseKeyword Kwcase
  _ <- sc
  _ <- parseSymbol SymBrackO
  _ <- sc
  pts <- parsePattern `sepBy` parseCommaSep
  _ <- sc
  _ <- parseSymbol SymBrackC
  loc <- getCurrLoc startPos
  pure (XCase loc pts)

parseShift :: SrcParser Term 
parseShift = do
  startPos <- getCurrPos
  _ <- parseSymbol SymBrackO
  _ <- sc
  t <- parseTerm
  _ <- sc
  _ <- parseSymbol SymColon
  _ <- sc
  eo <- parseEvaluationOrder
  _ <- parseSymbol SymBrackC
  loc <- getCurrLoc startPos
  case eo of 
    CBV -> pure (ShiftCBV loc t)
    CBN -> pure (ShiftCBN loc t)
    Any -> fail "Can only shift CBV or CBN"

parseXtor :: SrcParser Term
parseXtor = do
  startPos <- getCurrPos 
  nm <- parseXtorname 
  _ <- sc
  _ <- parseSymbol SymParensO
  _ <- sc
  args <- parseTerm `sepBy` parseCommaSep 
  _ <- sc
  _ <- parseSymbol SymParensC 
  loc <- getCurrLoc startPos
  pure $ Xtor loc nm args

parseLam :: SrcParser Term 
parseLam = do 
  startPos <- getCurrPos 
  _ <- parseSymbol SymBackSl <|> parseSymbol SymLambda
  v <- parseVariable
  _ <- sc
  _ <- parseSymbol SymDot
  _ <- sc
  t <- parseTerm
  loc <- getCurrLoc startPos
  pure $ Lam loc v t

parseVar :: SrcParser Term -- variable
parseVar = do
  startPos <- getCurrPos
  v <- parseVariable
  loc <- getCurrLoc startPos
  pure $ Var loc v


parsePattern :: SrcParser Pattern 
parsePattern = do 
  nm <- parseXtorname 
  _ <- sc
  args <- optionMaybe (parseParens (parseVariable `sepBy` parseCommaSep)) 
  _ <- sc
  _ <- parseSymbol SymEq 
  _ <- parseAngC
  _ <- sc
  c <- parseCommand
  case args of 
    Nothing -> pure $ Pattern {ptxt:nm,ptv:Nil,ptcmd:c}
    Just args' -> pure $ Pattern {ptxt:nm,ptv:args',ptcmd:c}

parseCommand :: SrcParser Command 
parseCommand = parseParens ((\_ -> parseC) unit) <|> (\_ -> parseC) unit

parseC :: SrcParser Command 
parseC = 
  (\_ -> parseErr)            unit <|> 
  (\_ -> parseDone)           unit <|>
  (\_ -> parseCut)            unit <|> 
  (\_ -> parseCaseOf)         unit <|>
  (\_ -> try parseCutCBV)     unit <|>
  (\_ -> try parseCutCBN)     unit <|>
  (\_ -> try parsePrint)      unit <|> 
  (\_ -> try parsePrintAnnot) unit      

parseCaseOf :: SrcParser Command 
parseCaseOf = do 
  startPos <- getCurrPos 
  _ <- parseKeyword KwCase <|> parseKeyword Kwcase
  _ <- sc1 
  t <- parseTerm 
  _ <- sc1
  _ <- parseKeyword KwOf <|> parseKeyword Kwof
  _ <- sc 
  _ <- parseSymbol SymBrackO
  _ <- sc
  pts <- parsePattern `sepBy` parseCommaSep 
  _ <- sc 
  _ <- parseSymbol SymBrackC 
  loc <- getCurrLoc startPos 
  pure $ CaseOf loc t pts

parseCut :: SrcParser Command 
parseCut = do
  startPos <- getCurrPos
  _ <- parseAngO
  _ <- sc
  t <- parseTerm
  _ <- sc
  _ <- parseSymbol SymBar
  _ <- sc
  Tuple pol mty <- parseCutAnnot 
  _ <- sc
  _ <- parseSymbol SymBar
  _ <- sc
  u <- parseTerm
  _ <- sc
  _ <- parseAngC
  loc <- getCurrLoc startPos 
  case mty of 
    Nothing -> pure (Cut loc t pol u)
    Just ty -> pure (CutAnnot loc t ty pol u)

parseDone :: SrcParser Command 
parseDone = do
  startPos <- getCurrPos
  _ <- parseKeyword KwDone  
  loc <- getCurrLoc startPos 
  pure (Done loc)

parseErr :: SrcParser Command 
parseErr = do
  startPos <- getCurrPos
  _ <- parseKeyword KwError
  _ <- sc 
  _ <- parseSymbol SymQuot
  msg <- many (noneOf ['\"'])
  _ <- parseSymbol SymQuot
  loc <- getCurrLoc startPos
  pure (Err loc (charlsToStr msg))
  where 
    charlsToStr :: List Char -> String
    charlsToStr Nil = "" 
    charlsToStr (Cons c1 cs) = (singleton c1) <> charlsToStr cs


parsePrintAnnot :: SrcParser Command 
parsePrintAnnot = do 
  startPos <- getCurrPos
  _ <- parseKeyword KwPrint <|> parseKeyword Kwprint 
  _ <- sc 
  t <- parseTerm 
  _ <- sc 
  _ <- parseSymbol SymColon
  _ <- parseSymbol SymColon
  _ <- sc 
  ty <- parseTy
  loc <- getCurrLoc startPos
  pure (PrintAnnot loc t ty)

parsePrint :: SrcParser Command 
parsePrint = do
  startPos <- getCurrPos 
  _ <- parseKeyword KwPrint <|> parseKeyword Kwprint
  _ <- sc 
  t <- parseTerm
  _ <- sc 
  loc <- getCurrLoc startPos
  pure (Print loc t)

parseCutCBV :: SrcParser Command
parseCutCBV = do  
    startPos <- getCurrPos 
    t <- parseTerm
    _ <- sc
    _ <- parseAngC
    _ <- parseAngC
    _ <- sc
    mty <- optionMaybe $ try (do
      ty <- parseTy 
      _ <- sc 
      _ <- parseAngC
      _ <- parseAngC 
      _ <- sc 
      pure ty)
    u <- parseTerm
    loc <- getCurrLoc startPos 
    case mty of 
      Nothing -> pure (Cut loc t CBV u)
      Just ty -> pure (CutAnnot loc t ty CBV u)

parseCutCBN :: SrcParser Command
parseCutCBN = do  -- cut with <<
  startPos <- getCurrPos 
  t <- parseTerm 
  _ <- sc 
  _ <- parseAngO
  _ <- parseAngO
  _ <- sc
  mty <- optionMaybe $ try (do
    ty <- parseTy 
    _ <- sc
    _ <- parseAngO
    _ <- parseAngO 
    _ <- sc 
    pure ty)
  u <- parseTerm
  loc <- getCurrLoc startPos 
  case mty of 
    Nothing -> pure (Cut loc t CBN u)
    Just ty -> pure (CutAnnot loc t ty CBN u)

parseCutAnnot :: SrcParser (Tuple EvaluationOrder (Maybe Ty))
parseCutAnnot = try (do 
  ty <- parseTy 
  _ <- sc 
  _ <- parseSymbol SymColon
  _ <- sc 
  eo <- parseEvaluationOrder 
  pure (Tuple eo (Just ty))) 
  <|>
  (do
  eo <- parseEvaluationOrder 
  pure $ Tuple eo Nothing)
