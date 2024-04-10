module Syntax.Kinded.Substitution (
) where 

import Syntax.Kinded.Types 
import Syntax.Kinded.Terms
import Common

import Data.Map qualified as M

--------------------------------
-- Kind Variable Substitution --
--------------------------------
class SubstKndVars a where 
  substKndVars :: M.Map Kindvar EvaluationOrder -> a -> a 

instance SubstKndVars Kind where 
  substKndVars _ p@MkKind{} = p
  substKndVars varmap k@(MkKindVar v) = maybe k MkKind (M.lookup v varmap)

instance SubstKndVars Ty where 
  substKndVars varmap (TyVar v knd) = TyVar v (substKndVars varmap knd) 
  substKndVars varmap (TyDecl tyn args knd) = TyDecl tyn (substKndVars varmap <$> args) (substKndVars varmap knd)
  substKndVars varmap (TyShift ty knd) = TyShift (substKndVars varmap ty) (substKndVars varmap knd)
  substKndVars varmap (TyCo ty) = TyCo (substKndVars varmap ty) 
  substKndVars varmap (TyForall args ty) = TyForall args (substKndVars varmap ty)

instance SubstKndVars Command where 
  substKndVars varmap (Cut loc t pol u) = Cut loc (substKndVars varmap t) pol (substKndVars varmap u)
  substKndVars _ (Done loc) = Done loc
  substKndVars _ (Err loc msg) = Err loc msg
  substKndVars varmap (Print loc t) = Print loc (substKndVars varmap t)

instance SubstKndVars Term where 
  substKndVars varmap (Var loc v ty) = Var loc v (substKndVars varmap ty)
  substKndVars varmap (Mu loc v c ty) = Mu loc v (substKndVars varmap c) (substKndVars varmap ty)
  substKndVars varmap (Xtor loc nm args ty) = Xtor loc nm (substKndVars varmap <$> args) (substKndVars varmap ty)
  substKndVars varmap (XCase loc pts ty) = XCase loc (substKndVars varmap <$> pts) (substKndVars varmap ty)
  substKndVars varmap (ShiftCBV loc t ty) = ShiftCBV loc (substKndVars varmap t) (substKndVars varmap ty)
  substKndVars varmap (ShiftCBN loc t ty) = ShiftCBN loc (substKndVars varmap t) (substKndVars varmap ty)

instance SubstKndVars Pattern where 
  substKndVars varmap (MkPattern xt vars c) = MkPattern xt vars (substKndVars varmap c)
