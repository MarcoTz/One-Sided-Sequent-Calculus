module Syntax.Parsed.Program where 

import Common
import Syntax.Parsed.Terms

data TypeScheme = MkTypeScheme ![TypeVar] !Ty

data Ty = TyVar !TypeVar | TyDecl !TypeName ![Ty]
data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data Decl = 
  MkData    {dataName  :: !TypeName, dataArgs  :: ![(Variable,Pol)], dataPol :: !Pol, dataSig :: ![XtorSig]} 
  | MkVar   {varName   :: !Variable, varBody   :: !Term}
  | MkAnnot {annotName :: !Variable, annotType :: !TypeScheme} 


--data RecDecl  = MkRecDecl{recVar  :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
