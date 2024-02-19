module Typed.Program where 

import Common 
import Typed.Types
import Typed.Syntax

data Decl = 
  MkDataDecl{declNm :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
  | MkValDecl{valVar :: !Variable, valTy :: !Ty, valBd :: !Term}
  | MkRecDecl{recVar :: !Variable, recTy :: !Ty, recBd :: !Term}
  | MkEps
  | MkCoDecl !Decl

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 


