module Parser.Program (parseProgram) where 

import Parser.Definition
import Parser.Types
import Parser.Terms
import Parser.Lexer
import Parser.Keywords
import Parser.Symbols
import Syntax.Parsed.Program
import Syntax.Parsed.Terms
import Common

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Map qualified as M
import Data.Maybe (isNothing)

parseModuleDecl :: Parser Modulename
parseModuleDecl = (do 
  parseKeyword KwModule 
  space1 
  sc 
  parseModulename)
  <|> return (Modulename "")

parseProgram :: String -> Parser Program 
parseProgram src = do 
  nm <-parseModuleDecl
  sc
  decls <- manyTill (parseDecl <* sc) eof
  foldM foldFun (emptyProg nm src) decls
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
  startPos <- getCurrPos
  parseKeyword KwImport
  space1
  mn <- parseModulename
  parseSymbol SymSemi
  loc <- getCurrLoc startPos
  return (MkImport loc mn)

parseTypeAnnot :: Parser AnnotDecl 
parseTypeAnnot = do
  startPos <- getCurrPos
  nm <- parseVariable 
  sc
  parseSymbol SymColon
  parseSymbol SymColon
  sc
  ty <- parseKindedTy
  sc
  parseSymbol SymSemi
  loc <- getCurrLoc startPos
  return $ MkAnnot loc nm ty 

parseVarDecl :: Parser VarDecl 
parseVarDecl = do 
  startPos <- getCurrPos 
  nm <- parseVariable 
  sc
  parseSymbol SymColon
  parseSymbol SymEq
  sc
  t <- parseTerm
  sc
  parseSymbol SymSemi
  loc <- getCurrLoc startPos
  return (MkVar loc nm t)

parseRecDecl :: Parser RecDecl 
parseRecDecl = do 
  parseKeyword KwRec
  sc
  (MkVar loc nm t) <- parseVarDecl
  return (MkRec loc nm t)

parseDataDecl :: Parser DataDecl 
parseDataDecl = do 
  startPos <- getCurrPos
  isco <- parseDataCodata
  space1
  nm <- parseTypename 
  sc
  args <- parseTyArgs
  sc
  parseSymbol SymBrackO 
  sc 
  xtors <- parseXtorSig `sepBy` parseCommaSep
  sc
  parseSymbol SymBrackC
  loc <- getCurrLoc startPos
  return $ MkData loc nm args isco xtors


parseXtorSig :: Parser XtorSig
parseXtorSig = do 
  startPos <- getCurrPos
  nm <- parseXtorname 
  args <- parseParens (parseTy `sepBy` parseCommaSep) <|> return [] 
  loc <- getCurrLoc startPos
  return $ MkXtorSig loc nm args 
