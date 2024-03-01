module Parser.Program where 

import Parser.Definition
import Parser.Types
import Parser.Terms
import Parser.Lexer
import Parser.Keywords
import Parser.Symbols
import Syntax.Parsed.Program

import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace 
import Pretty.Types ()
import Pretty.Terms ()

parseProgram :: Parser Program 
parseProgram = do 
  parseKeyword KwModule
  space1 
  sc
  mn <- some alphaNumChar
  trace ("parsed module name " <> mn) $ return ()
  sc
  decls <- manyTill (parseDecl <* sc) (sc >> eof)
  let prog = foldr foldFun emptyProg decls
  return prog 
  where 
    foldFun :: ParseDecl -> Program -> Program 
    foldFun (MkD decl) prog = addDeclProgram decl prog 
    foldFun (MkV var) prog = addVarProgram var prog 
    foldFun (MkA annot) prog = addAnnotProgram annot prog


parseDecl :: Parser ParseDecl 
parseDecl = do  
  (MkD <$> parseDataDecl) <|>  (MkV<$> try parseVarDecl) <|> (MkA <$> try parseTypeAnnot)

parseTypeAnnot :: Parser AnnotDecl 
parseTypeAnnot = do
  nm <- some alphaNumChar
  trace ("parsed annotation name " <> nm) $ return ()
  sc
  parseSymbol SymColon
  parseSymbol SymColon
  sc
  ty <- parseTypeScheme
  sc
  trace ("parsed annotation type " <> show ty)$ return ()
  trace ("parsed annotation " <> show nm <> ":" <> show ty) $ return ()
  return (MkAnnot nm ty)

parseVarDecl :: Parser VarDecl 
parseVarDecl = do 
  nm <- some alphaNumChar
  trace ("parsed variable name " <> nm ) $ return ()
  sc
  parseSymbol SymColon
  parseSymbol SymEq
  sc
  trace ("parsed variable " <> nm <> ":=" ) $ return ()
  t <- parseTerm
  sc
  parseSymbol SymSemi
  trace ("parsed variable declaration " <> show nm <> " = " <> show t)  $ return ()
  return (MkVar nm t)

parseDataDecl :: Parser DataDecl 
parseDataDecl = do 
  parseKeyword KwData
  space1
  nm <- some alphaNumChar
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
 nm <- some alphaNumChar
 MkXtorSig nm <$> parseXtorSigArgs
 
parseXtorSigArgs :: Parser [Ty]
parseXtorSigArgs = (do 
  parseSymbol SymParensO
  vars <- parseTy `sepBy` (parseSymbol SymComma >> sc) 
  parseSymbol SymParensC
  return vars)
  <|>
  return []
