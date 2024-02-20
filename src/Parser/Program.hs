module Parser.Program where 

import Parser.Definition
import Parser.Types
import Parser.Terms
import Parser.Lexer
import Parser.Keywords
import Parser.Symbols
import Untyped.Program

import Text.Megaparsec
import Text.Megaparsec.Char


parseProgram :: Parser Program
parseProgram = do 
  parseKeyword KwModule
  space1 
  _ <- some alphaNumChar
  space
  decls <- manyTill (parseDecl <* space) eof
  let (pgD,pgT) = foldr (\eit (dts,tms) -> case eit of Left d -> (d:dts,tms); Right t -> (dts,t:tms)) ([],[]) decls
  return $ MkProgram pgD pgT 


parseDecl :: Parser (Either DataDecl TermDecl)
parseDecl = (Left <$> parseDataDecl) <|> (Right <$> parseTermDecl)

parseTermDecl :: Parser TermDecl
parseTermDecl = do 
  parseKeyword KwVal
  space1 
  nm <- some alphaNumChar
  space
  parseSymbol SymEq
  t <- parseTerm
  parseSymbol SymSemi
  return (MkTermDecl nm t)

parseDataDecl :: Parser DataDecl 
parseDataDecl = do 
  parseKeyword KwData
  space1
  nm <- some alphaNumChar
  space
  args <- parseTyArgs
  space
  parseSymbol SymColon
  space
  pol <- parsePol
  space
  parseSymbol SymBrackO 
  space 
  xtors <- parseXtorSig `sepBy` (parseSymbol SymComma >> space)
  space
  parseSymbol SymBrackC
  return MkDataDecl{declNm=nm, declArgs=args,declPol=pol, declSig=xtors}


parseXtorSig :: Parser XtorSig
parseXtorSig = do 
 nm <- some alphaNumChar
 MkXtorSig nm <$> parseXtorArgs
 
parseXtorArgs :: Parser [Ty]
parseXtorArgs = (do 
  parseSymbol SymParensO
  vars <- parseTy `sepBy` (parseSymbol SymComma >> space) 
  parseSymbol SymParensC
  return vars)
  <|>
  return []
