module Syntax.Parsed.Program (
  XtorSig (..),
  AnnotDecl (..),
  RecDecl (..),
  VarDecl (..),
  DataDecl (..),
  Import (..),
  Program (..),
  addRecProgram,
  setMainProgram,
  addImportProgram,
  addAnnotProgram,
  addVarProgram,
  addDeclProgram,
  setSrcProgram,
  emptyProg,
) where 

import Common
import Loc 
import Syntax.Parsed.Terms
import Syntax.Parsed.Types

import Data.Map qualified as M

data XtorSig = MkXtorSig{sigPos :: !Loc, sigName :: !Xtorname, sigArgs :: ![Ty]} 
  deriving (Eq,Ord)
instance HasLoc XtorSig where 
  getLoc = sigPos
  setLoc loc (MkXtorSig _ nm args) = MkXtorSig loc nm args

data DataDecl  = MkData   {declPos   :: !Loc, declName   :: !Typename, declArgs  :: ![Polvar], dataPol   :: !Pol, declXtors :: ![XtorSig]} 
  deriving (Eq,Ord)
instance HasLoc DataDecl where 
  getLoc = declPos
  setLoc loc (MkData _ nm args pol xtors) = MkData loc nm args pol xtors
data VarDecl   = MkVar    {varPos    :: !Loc, varName    :: !Variable, varBody   :: !Term}
  deriving (Eq,Ord)
instance HasLoc VarDecl where 
  getLoc = varPos
  setLoc loc (MkVar _ nm bd) = MkVar loc nm bd

data RecDecl   = MkRec    {recPos    :: !Loc, recName    :: !Variable, recBody   :: !Term}
  deriving (Eq,Ord)
instance HasLoc RecDecl where 
  getLoc = recPos 
  setLoc loc (MkRec _ nm bd) = MkRec loc nm bd

data AnnotDecl = MkAnnot  {annotPos  :: !Loc, annotName  :: !Variable, annotType :: !PolTy} 
  deriving (Eq,Ord)
instance HasLoc AnnotDecl where 
  getLoc = annotPos 
  setLoc loc (MkAnnot _ nm ty) = MkAnnot loc nm ty

data Import = MkImport    {importPos :: !Loc, importName :: !Modulename }
  deriving (Eq,Ord)
instance HasLoc Import where 
  getLoc = importPos
  setLoc loc (MkImport _ nm) = MkImport loc nm

data Program = MkProgram { 
  progName    :: !Modulename, 
  progDecls   :: !(M.Map Typename DataDecl), 
  progVars    :: !(M.Map Variable VarDecl), 
  progRecs    :: !(M.Map Variable RecDecl),
  progAnnots  :: !(M.Map Variable AnnotDecl),
  progImports :: ![Import],
  progMain    :: !(Maybe Command),
  progSrc     :: !String}
  deriving (Eq,Ord)

emptyProg :: Modulename -> String -> Program 
emptyProg mn = MkProgram mn M.empty M.empty M.empty M.empty [] Nothing 

addDeclProgram :: DataDecl -> Program -> Program 
addDeclProgram decl prog = prog { progDecls = M.insert (declName decl) decl (progDecls prog) }

addVarProgram :: VarDecl -> Program -> Program 
addVarProgram var prog = prog { progVars = M.insert (varName var) var (progVars prog) }

addRecProgram :: RecDecl -> Program -> Program 
addRecProgram rec prog = prog { progRecs = M.insert (recName rec) rec (progRecs prog) }

addAnnotProgram :: AnnotDecl -> Program -> Program 
addAnnotProgram annot prog = prog { progAnnots = M.insert (annotName annot) annot (progAnnots prog) }

addImportProgram :: Import -> Program -> Program
addImportProgram imp prog = prog { progImports = imp : progImports prog }

setMainProgram :: Command -> Program -> Program
setMainProgram c prog = prog { progMain = Just c }

setSrcProgram :: String -> Program -> Program 
setSrcProgram src prog = prog { progSrc = src }

--newtype Codecl = MkCo DataDecl 
