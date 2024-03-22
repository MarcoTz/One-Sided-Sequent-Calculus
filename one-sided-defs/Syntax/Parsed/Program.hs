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
import Syntax.Parsed.Terms
import Syntax.Parsed.Types

import Data.Map qualified as M

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 
  deriving (Eq,Ord)

data DataDecl  = MkData   {declName   :: !TypeName, declArgs  :: ![PolVar], dataPol   :: !Pol, declXtors :: ![XtorSig]} 
  deriving (Eq,Ord)
data VarDecl   = MkVar    {varName    :: !Variable, varBody   :: !Term}
  deriving (Eq,Ord)
data RecDecl   = MkRec    {recName    :: !Variable, recBody   :: !Term}
  deriving (Eq,Ord)
data AnnotDecl = MkAnnot  {annotName  :: !Variable, annotType :: !PolTy} 
  deriving (Eq,Ord)
newtype Import = MkImport {importName :: Modulename }
  deriving (Eq,Ord)

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

--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
