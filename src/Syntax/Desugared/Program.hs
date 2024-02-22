module Syntax.Desugared.Program where 

import Common
import Syntax.Desugared.Terms
import Syntax.Desugared.Types


data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl = MkDataDecl{declNm :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
data VarDecl = MkVarDecl {varNm :: !Variable, varBd :: !Term}

data Program = MkProgram { progDecls :: ![DataDecl], progVars :: ![VarDecl]}
