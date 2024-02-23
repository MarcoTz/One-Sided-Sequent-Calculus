module Syntax.Typed.Program where 

import Common 
import Syntax.Typed.Types
import Syntax.Typed.Terms

import Data.Map qualified as M
import Data.Maybe (fromMaybe)

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
  substVars :: M.Map Variable Ty -> a -> a 

instance SubstVars XtorSig where 
  substVars varmap (MkXtorSig nm args) = MkXtorSig nm (substVars varmap <$> args)

instance SubstVars Ty where 
  substVars varmap ty@(TyVar v) = fromMaybe ty (M.lookup v varmap) 
  substVars varmap (TyDecl tyn args) = TyDecl tyn (substVars varmap <$> args)
  substVars varmap (TyShift ty) = TyShift (substVars varmap ty)
  substVars varmap (TyCo ty) = TyCo (substVars varmap ty)

instance SubstVars Term where 
  substVars varmap (Var v ty) = Var v (substVars varmap ty)
  substVars varmap (Mu v c ty) = Mu v (substVars varmap c) (substVars varmap ty)
  substVars varmap (Xtor nm args ty) = Xtor nm (substVars varmap <$> args) (substVars varmap ty)
  substVars varmap (XCase pts ty) = XCase (substVars varmap <$> pts) (substVars varmap ty)
  substVars varmap (Shift t ty) = Shift (substVars varmap t) (substVars varmap ty)
  substVars varmap (Lam v t ty) = Lam v (substVars varmap t) (substVars varmap ty)

instance SubstVars Pattern where 
  substVars varmap (MkPattern xt vars c) = MkPattern xt vars (substVars varmap c)

instance SubstVars Command where 
  substVars varmap (Cut t pol u) = Cut (substVars varmap t) pol (substVars varmap u) 
  substVars _ Done = Done
