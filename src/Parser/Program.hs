module Parser.Program where 

import Parser.Definition
import Parser.Types
import Parser.Lexer
import Parser.Keywords
import Parser.Symbols
import Untyped.Program

import Text.Megaparsec
import Text.Megaparsec.Char


parseProgram :: Parser [DataDecl]
parseProgram = do 
  parseKeyword KwModule
  space1 
  _ <- some alphaNumChar
  space
  manyTill (parseDecl <* space) eof


parseDecl :: Parser DataDecl 
parseDecl = do 
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
