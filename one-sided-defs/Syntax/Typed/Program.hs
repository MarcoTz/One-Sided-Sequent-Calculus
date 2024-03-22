module Syntax.Typed.Program (
  Program (..),
  RecDecl (..),
  VarDecl (..),
  DataDecl (..),
  XtorSig (..),
  addRecProgram,
  addVarProgram,
  addDeclProgram,
  emptyProg,
  isEmpty
) where 

import Common 
import Syntax.Typed.Types
import Syntax.Typed.Terms

import Data.Map qualified as M

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl  = MkData {declName :: !TypeName, declArgs :: ![PolVar], declPol :: !Pol, declXtors :: ![XtorSig]} 
data VarDecl   = MkVar  {varName  :: !Variable, varTy    :: !Ty,       varBody :: !Term}
data RecDecl   = MkRec  {recName  :: !Variable, recTy    :: !Ty,       recBody :: !Term}

data Program = MkProgram {
  progName  :: !Modulename, 
  progDecls :: !(M.Map TypeName DataDecl), 
  progVars  :: !(M.Map Variable VarDecl),
  progRecs  :: !(M.Map Variable RecDecl),
  progMain  :: !(Maybe Command)}

emptyProg :: Modulename -> Program
emptyProg nm = MkProgram nm M.empty M.empty M.empty Nothing

isEmpty :: Program -> Bool
isEmpty (MkProgram _ decls vars recs Nothing) =  null decls && null vars && null recs
isEmpty (MkProgram _ _ _ _ (Just _)) = False

addDeclProgram :: DataDecl -> Program -> Program
addDeclProgram decl (MkProgram nm decls vars recs main) = MkProgram nm (M.insert (declName decl) decl decls) vars recs main

addVarProgram :: VarDecl -> Program -> Program
addVarProgram var (MkProgram nm decls vars recs main) = MkProgram nm decls (M.insert (varName var) var vars) recs main

addRecProgram :: RecDecl -> Program -> Program
addRecProgram rec (MkProgram nm decls vars recs main) = MkProgram nm decls vars (M.insert (recName rec) rec recs) main

--newtype Codecl = MkCo DataDecl 
