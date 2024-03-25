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
  emptyProg,
) where 

import Common
import Loc 
import Syntax.Parsed.Terms
import Syntax.Parsed.Types

import Data.Map qualified as M

data XtorSig = MkXtorSig{sigPos :: !Loc, sigName :: !XtorName, sigArgs :: ![Ty]} 
  deriving (Eq,Ord)
instance HasLoc XtorSig where 
  getLoc = sigPos
  setLoc loc (MkXtorSig _ nm args) = MkXtorSig loc nm args

data DataDecl  = MkData   {declPos   :: !Loc, declName   :: !TypeName, declArgs  :: ![PolVar], dataPol   :: !Pol, declXtors :: ![XtorSig]} 
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
  progDecls   :: !(M.Map TypeName DataDecl), 
  progVars    :: !(M.Map Variable VarDecl), 
  progRecs    :: !(M.Map Variable RecDecl),
  progAnnots  :: !(M.Map Variable AnnotDecl),
  progImports :: ![Import],
  progMain    :: !(Maybe Command)} 
  deriving (Eq,Ord)

emptyProg :: Modulename -> Program 
emptyProg mn = MkProgram mn M.empty M.empty M.empty M.empty [] Nothing

addDeclProgram :: DataDecl -> Program -> Program 
addDeclProgram decl (MkProgram mn decls vars recs annots imports main) = MkProgram mn (M.insert (declName decl) decl decls) vars recs annots imports  main

addVarProgram :: VarDecl -> Program -> Program 
addVarProgram var (MkProgram mn decls vars recs annots imports main) = MkProgram mn decls (M.insert (varName var) var vars) recs annots imports main

addRecProgram :: RecDecl -> Program -> Program 
addRecProgram rec (MkProgram mn decls vars recs annots imports main) = MkProgram mn decls vars (M.insert (recName rec) rec recs) annots imports main

addAnnotProgram :: AnnotDecl -> Program -> Program 
addAnnotProgram annot (MkProgram mn decls vars recs annots imports main) = MkProgram mn decls vars recs (M.insert (annotName annot) annot annots) imports main

addImportProgram :: Import -> Program -> Program
addImportProgram imp (MkProgram mn decls vars recs annots imports main) = MkProgram mn decls vars recs annots (imp:imports) main

setMainProgram :: Command -> Program -> Program
setMainProgram c (MkProgram mn decls vars recs annots imports _) = MkProgram mn decls vars recs annots imports (Just c)

--newtype Codecl = MkCo DataDecl 
