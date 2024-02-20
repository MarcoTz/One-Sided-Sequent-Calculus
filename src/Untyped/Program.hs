module Untyped.Program where 

import Common
import Untyped.Syntax

data Ty = TyVar !TypeVar | TyDecl !TypeName ![Ty]
data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl = MkDataDecl{declNm :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
data VarDecl = MkVarDecl {varNm :: !Variable, varBd :: !Term}

data Program = MkProgram { progDat :: ![DataDecl], progVar :: ![VarDecl]}

--data RecDecl  = MkRecDecl{recVar  :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
