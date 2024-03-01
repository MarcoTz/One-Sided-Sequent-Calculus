module Syntax.Typed.Program where 

import Common 
import Syntax.Typed.Types
import Syntax.Typed.Terms

import Data.Map qualified as M

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl = MkDataDecl{declName :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
data VarDecl  = MkVarDecl {varName  :: !Variable, varTy    :: !TypeScheme,       varBd :: !Term}

data Program = MkProgram { progDecls :: !(M.Map TypeName DataDecl), progVars :: !(M.Map Variable VarDecl) }

emptyProg :: Program
emptyProg = MkProgram M.empty M.empty  

addDeclProgram :: DataDecl -> Program -> Program
addDeclProgram decl (MkProgram decls vars) = MkProgram (M.insert (declName decl) decl decls) vars

addVarProgram :: VarDecl -> Program -> Program
addVarProgram var (MkProgram decls vars) = MkProgram decls (M.insert (varName var) var vars)


--not implemented
--data RecDecl  = MkRecDecl{recVar :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
--
