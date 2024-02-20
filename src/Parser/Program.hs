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
parseProgram = some parseDecl 


parseDecl :: Parser DataDecl 
parseDecl = do 
  parseKeyword KwData
  space1
  nm <- some alphaNumChar
  args <- parseTyArgs
  space1
  parseSymbol SymColon
  space1
  pol <- parsePol
  space1
  _ <- optional eol
  parseSymbol SymBrackO
  _ <- optional eol
  space1 
  xtors <- parseXtorSig `sepBy` (parseSymbol SymComma >> optional space1)
  space1
  _ <- optional eol
  parseSymbol SymBrackC
  return MkDataDecl{declNm=nm, declArgs=args,declPol=pol, declSig=xtors}


parseXtorSig :: Parser XtorSig
parseXtorSig = do 
 nm <- some alphaNumChar
 MkXtorSig nm <$> parseXtorArgs
 
parseXtorArgs :: Parser [Ty]
parseXtorArgs = (do 
  parseSymbol SymParensO
  vars <- parseTy `sepBy` (parseSymbol SymComma >> optional space1) 
  parseSymbol SymParensC
  return vars)
  <|>
  return []
