module Syntax.Typed.FreeVars (
  freeVars,
  freshVar,
) where 

import Common 
import Syntax.Typed.Terms 
import Syntax.Typed.Types

import Data.Set qualified as S 

-------------------------------------
-------- Free Type Variables --------
-------------------------------------
--
instance FreeTypevars Ty where 
  freeTypevars (TyVar v) = S.singleton v 
  freeTypevars (TyDecl _ args) =  S.unions (freeTypevars <$> args)
  freeTypevars (TyShift ty) = freeTypevars ty
  freeTypevars (TyCo ty) = freeTypevars ty
  freeTypevars (TyForall args ty) = S.difference (freeTypevars ty) (S.fromList args)

instance FreeTypevars Term where 
  freeTypevars (Var _ _ ty) = freeTypevars ty
  freeTypevars (Mu _ _ c ty) = S.union (freeTypevars c) (freeTypevars ty)
  freeTypevars (Xtor _ _ args ty) = S.unions (freeTypevars ty :(freeTypevars <$> args))
  freeTypevars (XCase _ pts ty) = S.unions (freeTypevars ty : (freeTypevars <$> pts))
  freeTypevars (ShiftCBV _ t ty) = S.union (freeTypevars t) (freeTypevars ty)
  freeTypevars (ShiftCBN _ t ty) = S.union (freeTypevars t) (freeTypevars ty)

instance FreeTypevars Pattern where 
  freeTypevars (MkPattern _ _ c) = freeTypevars c

instance FreeTypevars Command where 
  freeTypevars (Cut _ t _ u) = S.union (freeTypevars t) (freeTypevars u)
  freeTypevars Done{} = S.empty
  freeTypevars Err{} = S.empty
  freeTypevars (Print _ t) = freeTypevars t
