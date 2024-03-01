module Syntax.Desugared.Program where 

import Common
import Syntax.Desugared.Terms
import Syntax.Desugared.Types


data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data Decl = 
  MkData  {dataName  :: !TypeName, declArgs :: ![(Variable,Pol)], dataPol :: !Pol, dataSig :: ![XtorSig]} 
  | MkVar {varName   :: !Variable, varTy    :: !(Maybe TypeScheme),       varBody :: !Term}
