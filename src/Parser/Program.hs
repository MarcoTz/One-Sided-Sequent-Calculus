module Parser.Program where 

import Parser.Definition
import Parser.Types
import Parser.Terms
import Parser.Lexer
import Parser.Keywords
import Parser.Symbols
import Syntax.Parsed.Program
import Syntax.Parsed.Terms

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Map qualified as M
import Data.Maybe (isNothing)


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
    foldFun prog (MkM mn) = do 
      guard (isNothing (progMain prog)) 
      return $ setMainProgram mn prog
    foldFun prog (MkR rec) = return $ addRecProgram rec prog

parseDecl :: Parser ParseDecl 
parseDecl = 
 (MkI <$> parseImport)       <|>
 (MkD <$> parseDataDecl)     <|>  
 (MkM <$> parseMain)         <|>
 (MkR <$> parseRecDecl)      <|>
 (MkV <$> try parseVarDecl)  <|> 
 (MkA <$> try parseTypeAnnot)

parseMain :: Parser Command 
parseMain = do 
  parseKeyword KwMain <|> parseKeyword Kwmain 
  sc 
  parseSymbol SymColon
  parseSymbol SymEq 
  sc 
  c <- parseCommand
  sc 
  parseSymbol SymSemi
  return c
  
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
  ty <- parsePolTy
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

parseRecDecl :: Parser RecDecl 
parseRecDecl = do 
  parseKeyword KwRec
  sc
  (MkVar nm t) <- parseVarDecl
  return (MkRec nm t)

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
  xtors <- parseXtorSig `sepBy` parseCommaSep
  sc
  parseSymbol SymBrackC
  return $ MkData nm args pol xtors


parseXtorSig :: Parser XtorSig
parseXtorSig = do 
 nm <- parseXtorName 
 args <- parseParens (parseTy `sepBy` parseCommaSep) <|> return [] 
 return $ MkXtorSig nm args 
