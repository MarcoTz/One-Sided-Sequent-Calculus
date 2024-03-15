module Syntax.Typed.Program where 

import Common 
import Syntax.Typed.Types
import Syntax.Typed.Terms

import Data.Map qualified as M

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl  = MkData {declName :: !TypeName, declArgs :: ![PolVar], declPol :: !Pol, declXtors :: ![XtorSig]} 
data VarDecl   = MkVar  {varName  :: !Variable, varTy    :: !Ty,       varBd   :: !Term}

data Program = MkProgram {
  progName :: !Modulename, 
  progDecls :: !(M.Map TypeName DataDecl), 
  progVars :: !(M.Map Variable VarDecl),
  progMain :: !(Maybe Command)}

emptyProg :: Modulename -> Program
emptyProg nm = MkProgram nm M.empty M.empty Nothing

isEmpty :: Program -> Bool
isEmpty (MkProgram _ decls vars Nothing) =  null decls && null vars
isEmpty (MkProgram _ _ _ (Just _)) = False

addDeclProgram :: DataDecl -> Program -> Program
addDeclProgram decl (MkProgram nm decls vars main) = MkProgram nm (M.insert (declName decl) decl decls) vars main

addVarProgram :: VarDecl -> Program -> Program
addVarProgram var (MkProgram nm decls vars main) = MkProgram nm decls (M.insert (varName var) var vars) main


--not implemented
--data RecDecl  = MkRecDecl{recVar :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
--
