module Syntax.Desugared.Program where 

import Common
import Syntax.Desugared.Terms
import Syntax.Desugared.Types

import Data.Map qualified as M


data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl = MkData  {declName  :: !TypeName, declArgs :: ![PolVar],              declPol :: !Pol,        declXtors :: ![XtorSig]} 
data VarDecl  = MkVar   {varName   :: !Variable, varArgs  :: ![(Variable,Maybe Ty)], varTy   :: !(Maybe Ty), varBody   :: !Term}

data Program = MkProgram { progName :: !Modulename, progDecls :: !(M.Map TypeName DataDecl), progVars :: !(M.Map Variable VarDecl)}  

emptyProg :: Modulename -> Program 
emptyProg nm = MkProgram nm M.empty M.empty

addDeclProgram :: DataDecl -> Program -> Program 
addDeclProgram decl (MkProgram nm dat vars) = MkProgram nm (M.insert (declName decl) decl dat) vars

addVarProgram :: VarDecl -> Program -> Program
addVarProgram var (MkProgram nm dat vars) = MkProgram nm dat (M.insert (varName var) var vars)
