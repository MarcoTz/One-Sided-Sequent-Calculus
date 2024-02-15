module Program where 

import Types 
import Syntax

data Decl = 
  MkDataDecl{declNm :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
  | MkValDecl{valVar :: !Variable, valTy :: !Ty, valBd :: !Term}
  | MkRecDecl{recVar :: !Variable, recTy :: !Ty, recBd :: !Term}
  | MkEps
  | MkCons !Decl !Decl
  | MkCoDecl !Decl

data XtorSig = MkXtorSig{sigName :: !String, sigArgs :: ![Ty]} 

data Env = MkEnv { envVars :: ![(Variable,Pol,Ty)], envTyVars :: ![(TypeVar, Pol)], envDecls :: ![Decl] }
