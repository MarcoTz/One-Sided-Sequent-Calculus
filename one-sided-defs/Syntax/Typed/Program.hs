module Syntax.Typed.Program (
  Program (..),
  RecDecl (..),
  VarDecl (..),
  DataDecl (..),
  XtorSig (..),
  addRecProgram,
  addVarProgram,
  addDeclProgram,
  setMainProgram,
  setSrcProgram,
  emptyProg,
  isEmpty
) where 

import Common 
import Loc
import Syntax.Typed.Types
import Syntax.Typed.Terms

import Data.Map qualified as M

data XtorSig = MkXtorSig{sigPos :: !Loc, sigName :: !Xtorname, sigArgs :: ![Ty]} 
instance HasLoc XtorSig where 
  getLoc = sigPos
  setLoc loc (MkXtorSig _ nm args) = MkXtorSig loc nm args 

data DataDecl  = MkData {declPos :: !Loc, declName :: !Typename, declArgs :: ![Polvar], declPol :: !Pol, declXtors :: ![XtorSig]} 
instance HasLoc DataDecl where 
  getLoc = declPos
  setLoc loc (MkData _ nm args pol xtors) = MkData loc nm args pol xtors

data VarDecl   = MkVar  {varPos  :: !Loc, varName  :: !Variable, varTy    :: !Ty,       varBody :: !Term}
instance HasLoc VarDecl where 
  getLoc = varPos
  setLoc loc (MkVar _ nm ty bd) = MkVar loc nm ty bd

data RecDecl   = MkRec  {recPos  :: !Loc, recName  :: !Variable, recTy    :: !Ty,       recBody :: !Term}
instance HasLoc RecDecl where 
  getLoc = recPos
  setLoc loc (MkRec _ nm ty t) = MkRec loc nm ty t

data Program = MkProgram {
  progName  :: !Modulename, 
  progDecls :: !(M.Map Typename DataDecl), 
  progVars  :: !(M.Map Variable VarDecl),
  progRecs  :: !(M.Map Variable RecDecl),
  progMain  :: !(Maybe Command),
  progSrc   :: !String
}

emptyProg :: Modulename -> Program
emptyProg nm = MkProgram nm M.empty M.empty M.empty Nothing ""

isEmpty :: Program -> Bool
isEmpty (MkProgram _ decls vars recs Nothing _) =  null decls && null vars && null recs
isEmpty (MkProgram _ _ _ _ (Just _) _) = False

addDeclProgram :: DataDecl -> Program -> Program
addDeclProgram decl prog = prog { progDecls = M.insert (declName decl) decl (progDecls prog) }

addVarProgram :: VarDecl -> Program -> Program
addVarProgram var prog = prog { progVars = M.insert (varName var) var (progVars prog) }

addRecProgram :: RecDecl -> Program -> Program
addRecProgram rec prog = prog { progRecs = M.insert (recName rec) rec (progRecs prog) }

setMainProgram :: Command -> Program -> Program
setMainProgram c prog = prog { progMain = Just c }

setSrcProgram :: String -> Program -> Program
setSrcProgram src prog = prog { progSrc = src }

--newtype Codecl = MkCo DataDecl 
