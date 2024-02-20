module Typed.Program where 

import Common 
import Typed.Types
import Typed.Syntax

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl = MkDataDecl{declNm :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
data VarDecl  = MkVarDecl{valVar :: !Variable, valTy :: !Ty, valBd :: !Term}

data Program = MkProgram { progDecls :: ![DataDecl], progVars :: ![VarDecl] }
emptyProg :: Program
emptyProg = MkProgram {progDecls = [], progVars = []}

--not implemented
--data RecDecl  = MkRecDecl{recVar :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
