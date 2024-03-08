module Parser.Program where 

import Parser.Definition
import Parser.Types
import Parser.Terms
import Parser.Lexer
import Parser.Keywords
import Parser.Symbols
import Syntax.Parsed.Program
import Syntax.Parsed.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Map qualified as M

parseProgram :: Parser Program 
parseProgram = do 
  parseKeyword KwModule
  space1 
  sc
  nm <- parseModulename
  sc
  decls <- manyTill (parseDecl <* sc) eof 
  foldM foldFun (emptyProg nm) decls
  where 
    foldFun :: Program -> ParseDecl -> Parser Program 
    foldFun prog (MkD decl) = do 
      let tyn = declName decl 
      guard (not (M.member tyn (progDecls prog)))
      return $ addDeclProgram decl prog 
    foldFun prog (MkV var)  = do
      let v = varName var 
      guard (not (M.member v (progVars prog)))
      return $ addVarProgram var prog 
    foldFun prog (MkA annot)= return $ addAnnotProgram annot prog
    foldFun prog (MkI imp) = return $ addImportProgram imp prog
    foldFun prog (MkS sugar) = return $ addSugarProgram sugar prog


parseDecl :: Parser ParseDecl 
parseDecl = 
 (MkI <$> parseImport)       <|>
 (MkD <$> parseDataDecl)     <|>  
 (MkS <$> parseSugar)        <|>
 (MkV <$> try parseVarDecl)  <|> 
 (MkA <$> try parseTypeAnnot) 

parseSugar :: Parser SugarDecl 
parseSugar = do 
  parseKeyword KwDefine
  sc 
  nm <- parseVariable
  sc
  args <- parseParens (parseVariable `sepBy` (parseSymbol SymComma >> sc))
  sc
  parseSymbol SymColon
  parseSymbol SymEq 
  sc 
  t <- parseTerm
  parseSymbol SymSemi
  return (MkSugar nm args t)


parseImport :: Parser Import
parseImport = do
  parseKeyword KwImport
  space1
  mn <- parseModulename
  parseSymbol SymSemi
  return (MkImport mn)

parseTypeAnnot :: Parser AnnotDecl 
parseTypeAnnot = do
  nm <- parseVariable 
  sc
  parseSymbol SymColon
  parseSymbol SymColon
  sc
  ty <- parseTy
  sc
  parseSymbol SymSemi
  return $ MkAnnot nm ty 

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
parseXtorSigArgs = parseParens (parseTy `sepBy` (parseSymbol SymComma >> sc)) <|>  return []
