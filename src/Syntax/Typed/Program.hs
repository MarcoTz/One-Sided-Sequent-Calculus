module Syntax.Typed.Program where 

import Common 
import Syntax.Typed.Types
import Syntax.Typed.Terms

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl = MkDataDecl{declNm :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
data VarDecl  = MkVarDecl{valVar :: !Variable, valTy :: !Ty, valBd :: !Term}

data Program = MkProgram { progDecls :: ![DataDecl], progVars :: ![VarDecl] }
emptyProg :: Program
emptyProg = MkProgram {progDecls = [], progVars = []}

addDeclToProgram :: DataDecl -> Program -> Program
addDeclToProgram decl (MkProgram decls vars) = MkProgram (decl:decls) vars

addVarToProgram :: VarDecl -> Program -> Program
addVarToProgram var (MkProgram decls vars) = MkProgram decls (var:vars)


--not implemented
--data RecDecl  = MkRecDecl{recVar :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
--
