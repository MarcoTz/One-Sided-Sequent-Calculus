module Syntax.Kinded.Substitution (
  SubstituteKindvars (..),
  SubstituteVariables (..),
  substituteVariable
) where 

import Syntax.Kinded.Types 
import Syntax.Kinded.Terms
import Common

import Data.Map qualified as M
import Data.Maybe (fromMaybe)

--------------------------------
-- Kind Variable Substitution --
--------------------------------
class SubstituteKindvars a where 
  substKindvars :: M.Map Kindvar EvaluationOrder -> a -> a 

instance SubstituteKindvars Kind where 
  substKindvars _ p@MkKind{} = p
  substKindvars varmap k@(MkKindVar v) = maybe k MkKind (M.lookup v varmap)

instance SubstituteKindvars Ty where 
  substKindvars varmap (TyVar v knd) = TyVar v (substKindvars varmap knd) 
  substKindvars varmap (TyDecl tyn args knd) = TyDecl tyn (substKindvars varmap <$> args) (substKindvars varmap knd)
  substKindvars varmap (TyShift ty knd) = TyShift (substKindvars varmap ty) (substKindvars varmap knd)
  substKindvars varmap (TyCo ty) = TyCo (substKindvars varmap ty) 
  substKindvars varmap (TyForall args ty) = TyForall args (substKindvars varmap ty)

instance SubstituteKindvars Command where 
  substKindvars varmap (Cut loc t pol u) = Cut loc (substKindvars varmap t) pol (substKindvars varmap u)
  substKindvars _ (Done loc) = Done loc
  substKindvars _ (Err loc msg) = Err loc msg
  substKindvars varmap (Print loc t) = Print loc (substKindvars varmap t)

instance SubstituteKindvars Term where 
  substKindvars varmap (Var loc v ty) = Var loc v (substKindvars varmap ty)
  substKindvars varmap (Mu loc v c ty) = Mu loc v (substKindvars varmap c) (substKindvars varmap ty)
  substKindvars varmap (Xtor loc nm args ty) = Xtor loc nm (substKindvars varmap <$> args) (substKindvars varmap ty)
  substKindvars varmap (XCase loc pts ty) = XCase loc (substKindvars varmap <$> pts) (substKindvars varmap ty)
  substKindvars varmap (ShiftCBV loc t ty) = ShiftCBV loc (substKindvars varmap t) (substKindvars varmap ty)
  substKindvars varmap (ShiftCBN loc t ty) = ShiftCBN loc (substKindvars varmap t) (substKindvars varmap ty)

instance SubstituteKindvars Pattern where 
  substKindvars varmap (MkPattern xt vars c) = MkPattern xt vars (substKindvars varmap c)

--------------------------------------------------------------
----------------- Term Variable Substitution -----------------
--------------------------------------------------------------

class SubstituteVariables a where 
  substVars :: M.Map Variable Term -> a -> a 

instance SubstituteVariables Term where 
  substVars varmap vt@(Var _ v _) = fromMaybe vt (M.lookup v varmap)
  substVars varmap (Mu loc v c ty) = Mu loc v (substVars (M.delete v varmap) c) ty
  substVars varmap (Xtor loc nm args ty) = Xtor loc nm (substVars varmap <$> args ) ty
  substVars varmap (XCase loc pts ty) = XCase loc (substVars varmap <$> pts) ty
  substVars varmap (ShiftCBV loc t ty) = ShiftCBV loc (substVars varmap t) ty 
  substVars varmap (ShiftCBN loc t ty) = ShiftCBN loc (substVars varmap t) ty 

instance SubstituteVariables Pattern where 
  substVars varmap (MkPattern xt vars c) = let newMap = foldr M.delete varmap  vars in MkPattern xt vars (substVars newMap c)

instance SubstituteVariables Command where 
  substVars varmap (Cut loc t eo u) = Cut loc (substVars varmap t) eo (substVars varmap u)
  substVars _ d@Done{} = d
  substVars _ e@Err{} = e
  substVars varmap (Print loc t) = Print loc (substVars varmap t)

substituteVariable :: SubstituteVariables a => Variable -> Term -> a -> a
substituteVariable substVar substT a = let varmap = M.fromList [(substVar,substT)] in substVars varmap a 
