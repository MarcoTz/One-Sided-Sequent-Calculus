module Parser.Program where 

import Parser.Definition
import Parser.Types
import Parser.Terms
import Parser.Lexer
import Parser.Keywords
import Parser.Symbols
import Syntax.Parsed.Program
import Syntax.Parsed.Types
import Errors

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Except 
import Control.Monad
import Data.Map qualified as M

import Pretty.Types ()
import Pretty.Terms ()

parseProgram :: Parser Program 
parseProgram = do 
  parseKeyword KwModule
  space1 
  sc
  _ <- some alphaNumChar
  sc
  decls <- manyTill (parseDecl <* sc) (sc >> eof)
  foldM foldFun emptyProg decls
  where 
    foldFun :: Program -> ParseDecl -> Parser Program 
    foldFun prog (MkD decl) = let tyn = declName decl in 
      if M.member tyn (progDecls prog) then throwError (ErrDuplDecl tyn "parseProgram") else return $ addDeclProgram decl prog 
    foldFun prog (MkV var)  = let v = varName var in 
      if M.member v (progVars prog) then throwError (ErrDuplVar v "parseProgram") else return $ addVarProgram var prog 
    foldFun prog (MkA annot)= return $ addAnnotProgram annot prog


parseDecl :: Parser ParseDecl 
parseDecl = do  
  (MkD <$> parseDataDecl) <|>  (MkV<$> try parseVarDecl) <|> (MkA <$> try parseTypeAnnot)

parseTypeAnnot :: Parser AnnotDecl 
parseTypeAnnot = do
  nm <- parseVariable 
  sc
  parseSymbol SymColon
  parseSymbol SymColon
  sc
  MkAnnot nm <$> parseTy

parseVarDecl :: Parser VarDecl 
parseVarDecl = do 
  nm <- parseVariable 
  sc
  parseSymbol SymColon
  parseSymbol SymEq
  sc
  t <- parseTerm
  sc
  parseSymbol SymSemi
  return (MkVar nm t)

parseDataDecl :: Parser DataDecl 
parseDataDecl = do 
  parseKeyword KwData
  space1
  nm <- parseTypeName 
  sc
  args <- parseTyArgs
  sc
  parseSymbol SymColon
  sc
  pol <- parsePol
  sc
  parseSymbol SymBrackO 
  sc 
  xtors <- parseXtorSig `sepBy` (parseSymbol SymComma >> sc)
  sc
  parseSymbol SymBrackC
  return $ MkData nm args pol xtors


parseXtorSig :: Parser XtorSig
parseXtorSig = do 
 nm <- parseXtorName 
 MkXtorSig nm <$> parseXtorSigArgs
 
parseXtorSigArgs :: Parser [Ty]
parseXtorSigArgs = (do 
  parseSymbol SymParensO
  vars <- parseTy `sepBy` (parseSymbol SymComma >> sc) 
  parseSymbol SymParensC
  return vars)
  <|>
  return []
