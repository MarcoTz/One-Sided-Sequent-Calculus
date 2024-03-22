module Syntax.Desugared.Program (
  XtorSig (..),
  VarDecl (..),
  RecDecl (..),
  DataDecl (..),
  Program (..),
  setMainProgram,
  addRecProgram,
  addVarProgram,
  addDeclProgram,
  emptyProg
) where 

import Common
import Syntax.Desugared.Terms
import Syntax.Desugared.Types

import Data.Map qualified as M


data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl = MkData {declName :: !TypeName, declArgs :: ![PolVar],      declPol :: !Pol, declXtors :: ![XtorSig]} 
data VarDecl  = MkVar  {varName  :: !Variable, varTy    :: !(Maybe PolTy), varBody :: !Term}
data RecDecl  = MkRec  {recName  :: !Variable, recTy    :: !(Maybe PolTy), recBody :: !Term}

data Program = MkProgram { 
  progName  :: !Modulename, 
  progDecls :: !(M.Map TypeName DataDecl), 
  progVars  :: !(M.Map Variable VarDecl),
  progRecs  :: !(M.Map Variable RecDecl),
  progMain  :: !(Maybe Command)
}  

emptyProg :: Modulename -> Program 
emptyProg nm = MkProgram nm M.empty M.empty M.empty Nothing

addDeclProgram :: DataDecl -> Program -> Program 
addDeclProgram decl (MkProgram nm dat vars rec main) = MkProgram nm (M.insert (declName decl) decl dat) vars rec main

addVarProgram :: VarDecl -> Program -> Program
addVarProgram var (MkProgram nm dat vars rec main) = MkProgram nm dat (M.insert (varName var) var vars) rec main

addRecProgram :: RecDecl -> Program -> Program
addRecProgram rec (MkProgram nm dat vars recs main) = MkProgram nm dat vars (M.insert (recName rec) rec recs) main

setMainProgram :: Command -> Program -> Program 
setMainProgram c (MkProgram nm dat vars rec _) = MkProgram nm dat vars rec (Just c)
