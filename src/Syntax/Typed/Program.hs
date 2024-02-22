module Syntax.Typed.Program where 

import Common 
import Syntax.Typed.Types
import Syntax.Typed.Terms

import Data.Map qualified as M

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
class SubstVars a where 
  substVars :: a -> M.Map Variable Variable -> a 

instance SubstVars XtorSig where 
  substVars (MkXtorSig nm args) varmap = MkXtorSig nm ((`substVars` varmap) <$> args)

instance SubstVars Ty where 
 substVars ty@(TyVar v knd) varmap = case M.lookup v varmap of Nothing -> ty; Just v' -> TyVar v' knd
 substVars (TyDecl tyn args knd) varmap = TyDecl tyn ((`substVars` varmap) <$> args) knd 
 substVars (TyShift ty knd) varmap = TyShift (substVars ty varmap) knd
 substVars (TyCo ty knd) varmap = TyCo (substVars ty varmap) knd
