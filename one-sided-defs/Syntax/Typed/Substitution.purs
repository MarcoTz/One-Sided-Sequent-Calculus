module Syntax.Typed.Substitution ( 
  substTyvars
) where 

import Syntax.Typed.Types 
import Syntax.Typed.Program
import Syntax.Typed.Terms
import Common

import Data.Map qualified as M
import Data.Maybe (fromMaybe)


--------------------------------
-- Type Variable Substitution --
--------------------------------
class SubstituteTypevars a where 
  substTyvars :: M.Map Typevar Ty -> a -> a 

instance SubstituteTypevars XtorSig where 
  substTyvars varmap (MkXtorSig loc nm args) = MkXtorSig loc nm (substTyvars varmap <$> args)

instance SubstituteTypevars Ty where 
  substTyvars varmap ty@(TyVar v) = fromMaybe ty (M.lookup v varmap)
  substTyvars varmap (TyDecl tyn args) = TyDecl tyn (substTyvars varmap <$> args)
  substTyvars varmap (TyShift ty) = TyShift (substTyvars varmap ty)
  substTyvars varmap (TyCo ty) = TyCo (substTyvars varmap ty) 
  substTyvars varmap (TyForall vars ty) = let newmap = foldr M.delete varmap vars in TyForall vars (substTyvars newmap ty)

instance SubstituteTypevars Term where 
  substTyvars varmap (Var loc v ty) = Var loc v (substTyvars varmap ty)
  substTyvars varmap (Mu loc v c ty) = Mu loc v (substTyvars varmap c) (substTyvars varmap ty)
  substTyvars varmap (Xtor loc nm args ty) = Xtor loc nm (substTyvars varmap <$> args) (substTyvars varmap ty)
  substTyvars varmap (XCase loc pts ty) = XCase loc (substTyvars varmap <$> pts) (substTyvars varmap ty)
  substTyvars varmap (ShiftCBV loc t ty) = ShiftCBV loc (substTyvars varmap t) (substTyvars varmap ty)
  substTyvars varmap (ShiftCBN loc t ty) = ShiftCBN loc (substTyvars varmap t) (substTyvars varmap ty)

instance SubstituteTypevars Pattern where 
  substTyvars varmap (MkPattern xt vars c) = MkPattern xt vars (substTyvars varmap c)

instance SubstituteTypevars Command where 
  substTyvars varmap (Cut loc t pol u) = Cut loc (substTyvars varmap t) pol (substTyvars varmap u) 
  substTyvars _ (Done loc) = Done loc
  substTyvars _ (Err loc err) = Err loc err
  substTyvars varmap (Print loc t) = Print loc (substTyvars varmap t)
