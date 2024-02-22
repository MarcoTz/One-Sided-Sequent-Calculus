module Syntax.Desugared.Program where 

import Common
import Syntax.Desugared.Terms
import Syntax.Desugared.Types

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl = MkDataDecl{declNm :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
data VarDecl = MkVarDecl {varNm :: !Variable, varBd :: !Term}

data Program = MkProgram { progDecls :: ![DataDecl], progVars :: ![VarDecl]}

--data RecDecl  = MkRecDecl{recVar  :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
--
--
--data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

--data DataDecl = MkDataDecl{declNm :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
--data VarDecl  = MkVarDecl{valVar :: !Variable, valTy :: !Ty, valBd :: !Term}

--data Program = MkProgram { progDecls :: ![DataDecl], progVars :: ![VarDecl] }

--addDeclToProgram :: DataDecl -> Program -> Program
--addDeclToProgram decl (MkProgram decls vars) = MkProgram (decl:decls) vars

--addVarToProgram :: VarDecl -> Program -> Program
--addVarToProgram var (MkProgram decls vars) = MkProgram decls (var:vars)


--not implemented
--data RecDecl  = MkRecDecl{recVar :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
