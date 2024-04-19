module Parser.Program (parseProgram) where 

import Parser.Definition (SrcParser, ParseDecl(..)) 
import Parser.Lexer (sc, parseKeyword, parseSymbol, getCurrPos, getCurrLoc, parseParens, parseCommaSep)
import Parser.Common (parseModulename, parseVariable, parseDataCodata, parseXtorname, parseTypename)
import Parser.Keywords (Keyword(..))
import Parser.Symbols (Sym(..))
import Parser.Terms (parseCommand,parseTerm)
import Parser.Types (parseTy, parseTyArgs)
import Common (Modulename(..))
import Syntax.Parsed.Program (
  Program(..), DataDecl(..), Import (..), AnnotDecl(..), VarDecl(..), XtorSig(..),
  addDeclProgram, addVarProgram, addAnnotProgram, addImportProgram, setMainProgram, emptyProg)
import Syntax.Parsed.Terms (Command)

import Prelude (bind,pure, (<>), ($), (<$>), show, (<*))
import Data.Map (member)
import Data.List (List(..))
import Data.Maybe (Maybe(..),isJust)
import Data.Foldable (foldM)
import Parsing (fail)
import Parsing.Combinators (manyTill, try, sepBy, optionMaybe)
import Parsing.String (eof)
import Parsing.String.Basic (space)
import Control.Alt ((<|>))

parseModuleDecl :: SrcParser Modulename
parseModuleDecl = (do 
  _ <- parseKeyword KwModule 
  _ <- space
  _ <- sc 
  parseModulename)
  <|> pure (Modulename "")

parseProgram :: String -> SrcParser Program 
parseProgram src = do 
  nm <-parseModuleDecl
  _ <- sc
  decls <- manyTill 
    (do
     decl <- parseDecl
     _ <- sc
     pure decl) eof
  foldM foldFun (emptyProg nm src) decls
  where 
    foldFun :: Program -> ParseDecl -> SrcParser Program 
    foldFun p@(Program prog) (MkD d@(DataDecl decl)) = do 
      let tyn = decl.declName 
      if member tyn (prog.progDecls) then fail ("multiple declarations for type " <> show tyn) else pure $ addDeclProgram d p
    foldFun p@(Program prog) (MkV v@(VarDecl var))  = do
      let nm = var.varName
      if member nm (prog.progVars) then fail ("multiple declarations for variable " <> show v) else pure $ addVarProgram v p
    foldFun prog (MkA annot) = pure $ addAnnotProgram annot prog
    foldFun prog (MkI imp) = pure $ addImportProgram imp prog
    foldFun p@(Program prog) (MkM mn) = do 
      if isJust (prog.progMain) then fail "multiple definitions of main" else pure $ setMainProgram mn p

parseDecl :: SrcParser ParseDecl 
parseDecl = 
 (MkI <$> parseImport)       <|>
 (MkD <$> parseDataDecl)     <|>  
 (MkM <$> parseMain)         <|>
 (MkV <$> try parseVarDecl)  <|> 
 (MkA <$> try parseTypeAnnot)

parseMain :: SrcParser Command 
parseMain = do 
  _ <- parseKeyword KwMain <|> parseKeyword Kwmain 
  _ <- sc 
  _ <- parseSymbol SymColon
  _ <- parseSymbol SymEq 
  _ <- sc 
  c <- parseCommand
  _ <- sc 
  _ <- parseSymbol SymSemi
  pure c
  
parseImport :: SrcParser Import
parseImport = do
  startPos <- getCurrPos
  _ <- parseKeyword KwImport
  _ <- space
  _ <- sc
  mn <- parseModulename
  _ <- parseSymbol SymSemi
  loc <- getCurrLoc startPos
  pure $ Import {importPos:loc, importName:mn}

parseTypeAnnot :: SrcParser AnnotDecl 
parseTypeAnnot = do
  startPos <- getCurrPos
  nm <- parseVariable 
  _ <- sc
  _ <- parseSymbol SymColon
  _ <- parseSymbol SymColon
  _ <- sc
  ty <- parseTy
  _ <- sc
  _ <- parseSymbol SymSemi
  loc <- getCurrLoc startPos
  pure $ AnnotDecl {annotPos:loc, annotName:nm, annotType:ty}

parseVarDecl :: SrcParser VarDecl 
parseVarDecl = do 
  startPos <- getCurrPos 
  isRec <- optionMaybe (parseKeyword KwRec <* sc)
  nm <- parseVariable 
  _ <- sc
  _ <- parseSymbol SymColon
  _ <- parseSymbol SymEq
  _ <- sc
  t <- parseTerm
  _ <- sc
  _ <- parseSymbol SymSemi
  loc <- getCurrLoc startPos
  pure $ VarDecl {varPos:loc, varName:nm, varIsRec:isJust isRec, varBody:t}

parseDataDecl :: SrcParser DataDecl 
parseDataDecl = do 
  startPos <- getCurrPos
  isco <- parseDataCodata
  _ <- space
  _ <- sc
  nm <- parseTypename 
  _ <- sc
  args <- parseTyArgs
  _ <- sc
  _ <- parseSymbol SymBrackO 
  _ <- sc 
  xtors <- parseXtorSig `sepBy` parseCommaSep
  _ <- sc
  _ <- parseSymbol SymBrackC
  loc <- getCurrLoc startPos
  pure $ DataDecl {declPos:loc, declName:nm, declArgs:args, declType:isco, declXtors:xtors}


parseXtorSig :: SrcParser XtorSig
parseXtorSig = do 
  startPos <- getCurrPos
  nm <- parseXtorname 
  args <- optionMaybe (parseParens (parseTy `sepBy` parseCommaSep))
  loc <- getCurrLoc startPos
  case args of 
    Nothing -> pure $ XtorSig {sigPos:loc, sigName:nm, sigArgs:Nil}
    Just args' -> pure $ XtorSig {sigPos:loc, sigName:nm, sigArgs:args'}

