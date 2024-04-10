module Syntax.Kinded.FreeVars (
  FreeKindvars (..)
) where 

import Common 
import Syntax.Kinded.Terms 
import Syntax.Kinded.Types

import Data.Set qualified as S 


class FreeKindvars a where 
  freeKindvars :: a -> S.Set Kindvar

instance FreeKindvars Kind where 
  freeKindvars (MkKind _) = S.empty 
  freeKindvars (MkKindVar v) = S.singleton v 

instance FreeKindvars Ty where
  freeKindvars (TyVar _ knd) = freeKindvars knd
  freeKindvars (TyDecl _ args knd) = S.unions (freeKindvars knd : (freeKindvars <$> args))
  freeKindvars (TyShift ty knd) = S.union (freeKindvars ty) (freeKindvars knd)
  freeKindvars (TyCo ty) = freeKindvars ty
  freeKindvars (TyForall _ ty) = freeKindvars ty

instance FreeKindvars Term where
  freeKindvars (Var _ _ ty)        = freeKindvars ty
  freeKindvars (Mu _ _ c ty)       = S.union (freeKindvars ty) (freeKindvars c)
  freeKindvars (Xtor _ _ args ty)  = S.unions (freeKindvars ty : (freeKindvars <$> args))
  freeKindvars (XCase _ pts ty)    = S.unions (freeKindvars ty : (freeKindvars <$> pts))
  freeKindvars (ShiftCBV _ t ty)   = S.union (freeKindvars t) (freeKindvars ty)
  freeKindvars (ShiftCBN _ t ty)   = S.union (freeKindvars t) (freeKindvars ty)

instance FreeKindvars Pattern where 
  freeKindvars (MkPattern _ _ cmd) = freeKindvars cmd 

instance FreeKindvars Command where 
  freeKindvars (Cut _ t1 _ t2) = S.union (freeKindvars t1) (freeKindvars t2) 
  freeKindvars Done{} = S.empty
  freeKindvars Err{}  = S.empty
  freeKindvars (Print _ t) = freeKindvars t
