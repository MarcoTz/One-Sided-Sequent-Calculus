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
import Loc
import Syntax.Desugared.Terms
import Syntax.Desugared.Types

import Data.Map qualified as M


data XtorSig = MkXtorSig{sigPos :: !Loc, sigName :: !XtorName, sigArgs :: ![Ty]} 
instance HasLoc XtorSig where 
  getLoc = sigPos
  setLoc loc (MkXtorSig _ nm args) = MkXtorSig loc nm args

data DataDecl = MkData {declPos :: !Loc, declName :: !TypeName, declArgs :: ![PolVar],      declPol :: !Pol, declXtors :: ![XtorSig]} 
instance HasLoc DataDecl where 
  getLoc = declPos 
  setLoc loc (MkData _ nm args pol xtors) = MkData loc nm args pol xtors

data VarDecl  = MkVar  {varPos  :: !Loc, varName  :: !Variable, varTy    :: !(Maybe PolTy), varBody :: !Term}
instance HasLoc VarDecl where 
  getLoc = varPos 
  setLoc loc (MkVar _ nm ty bd) = MkVar loc nm ty bd

data RecDecl  = MkRec  {recPos  :: !Loc, recName  :: !Variable, recTy    :: !(Maybe PolTy), recBody :: !Term}
instance HasLoc RecDecl where 
  getLoc = recPos
  setLoc loc (MkRec _ nm ty bd) = MkRec loc nm ty bd

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
