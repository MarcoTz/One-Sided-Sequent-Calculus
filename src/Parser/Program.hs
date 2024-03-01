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


parseProgram :: Parser Program 
parseProgram = do 
  parseKeyword KwModule
  space1 
  sc
  _ <- some alphaNumChar
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
  nm <- parseVariable 
  sc
  parseSymbol SymColon
  parseSymbol SymColon
  sc
  ty <- parseTypeScheme <|> (generalize <$> parseTy)
  sc
  return (MkAnnot nm ty)

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
