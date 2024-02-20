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


parseProgram :: Parser Program
parseProgram = do 
  parseKeyword KwModule
  space1 
  sc
  _ <- some alphaNumChar
  sc
  decls <- manyTill (parseDecl <* sc) (sc >> eof)
  let (pgD,pgT) = foldr (\eit (dts,tms) -> case eit of Left d -> (d:dts,tms); Right t -> (dts,t:tms)) ([],[]) decls
  return $ MkProgram pgD pgT 


parseDecl :: Parser (Either DataDecl VarDecl)
parseDecl = (Left <$> parseDataDecl) <|> (Right <$> parseVarDecl)

parseVarDecl :: Parser VarDecl
parseVarDecl = do 
  parseKeyword KwVal
  space1 
  sc
  nm <- some alphaNumChar
  sc
  parseSymbol SymEq
  sc
  t <- parseTerm
  parseSymbol SymSemi
  return (MkVarDecl nm t)

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
  return MkDataDecl{declNm=nm, declArgs=args,declPol=pol, declSig=xtors}


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
