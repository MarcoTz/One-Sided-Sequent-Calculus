module Syntax.Typed.Substitution (
  substVar,
  substTyVars
) where 

import Syntax.Typed.Types 
import Syntax.Typed.Program
import Syntax.Typed.Terms
import Syntax.Typed.FreeVars
import Common
import Loc

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromMaybe)


--------------------------------
-- Kind Variable Substitution --
--------------------------------
--class SubstKndVars a where 
--  substKndVars :: M.Map KindVar Pol -> a -> a 
--
--instance SubstKndVars Kind where 
--  substKndVars _ p@MkKind{} = p
--  substKndVars varmap k@(MkKindVar v) = maybe k MkKind (M.lookup v varmap)
--
--instance SubstKndVars Ty where 
--  substKndVars varmap (TyVar v knd) = TyVar v (substKndVars varmap knd) 
--  substKndVars varmap (TyDecl tyn args knd) = TyDecl tyn (substKndVars varmap <$> args) (substKndVars varmap knd)
--  substKndVars varmap (TyShift ty knd) = TyShift (substKndVars varmap ty) (substKndVars varmap knd)
--  substKndVars varmap (TyCo ty knd) = TyCo (substKndVars varmap ty) (substKndVars varmap knd)
--
--instance SubstKndVars Command where 
--  substKndVars varmap (Cut t pol u) = Cut (substKndVars varmap t) pol (substKndVars varmap u)
--  substKndVars _ Done = Done
--
--instance SubstKndVars Term where 
--  substKndVars varmap (Var v ty) = Var v (substKndVars varmap ty)
--  substKndVars varmap (Mu v c ty) = Mu v (substKndVars varmap c) (substKndVars varmap ty)
--  substKndVars varmap (Xtor nm args ty) = Xtor nm (substKndVars varmap <$> args) (substKndVars varmap ty)
--  substKndVars varmap (XCase pts ty) = XCase (substKndVars varmap <$> pts) (substKndVars varmap ty)
--  substKndVars varmap (Shift t ty) = Shift (substKndVars varmap t) (substKndVars varmap ty)
--  substKndVars varmap (Lam v t ty) = Lam v (substKndVars varmap t) (substKndVars varmap ty)
--
--instance SubstKndVars Pattern where 
--  substKndVars varmap (MkPattern xt vars c) = MkPattern xt vars (substKndVars varmap c)

--------------------------------
-- Type Variable Substitution --
--------------------------------
class SubstTyVars a where 
  substTyVars :: M.Map Polvar Ty -> a -> a 

instance SubstTyVars XtorSig where 
  substTyVars varmap (MkXtorSig loc nm args) = MkXtorSig loc nm (substTyVars varmap <$> args)

instance SubstTyVars Ty where 
  substTyVars varmap ty@(TyVar v pol) = fromMaybe ty (M.lookup (Polvar v pol) varmap) 
  substTyVars varmap (TyDecl tyn args knd) = TyDecl tyn (substTyVars varmap <$> args) knd
  substTyVars varmap (TyShift ty knd) = TyShift (substTyVars varmap ty) knd
  substTyVars varmap (TyCo ty) = TyCo (substTyVars varmap ty) 
  substTyVars varmap (TyForall vars ty) = let newmap = foldr (\v m -> M.delete (Polvar v Neg) (M.delete (Polvar v Pos) m)) varmap vars in TyForall vars (substTyVars newmap ty)

instance SubstTyVars Term where 
  substTyVars varmap (Var loc v ty) = Var loc v (substTyVars varmap ty)
  substTyVars varmap (Mu loc v c ty) = Mu loc v (substTyVars varmap c) (substTyVars varmap ty)
  substTyVars varmap (Xtor loc nm args ty) = Xtor loc nm (substTyVars varmap <$> args) (substTyVars varmap ty)
  substTyVars varmap (XCase loc pts ty) = XCase loc (substTyVars varmap <$> pts) (substTyVars varmap ty)
  substTyVars varmap (ShiftPos loc t ty) = ShiftPos loc (substTyVars varmap t) (substTyVars varmap ty)
  substTyVars varmap (ShiftNeg loc v t ty) = ShiftNeg loc v (substTyVars varmap t) (substTyVars varmap ty)

instance SubstTyVars Pattern where 
  substTyVars varmap (MkPattern xt vars c) = MkPattern xt vars (substTyVars varmap c)

instance SubstTyVars Command where 
  substTyVars varmap (Cut loc t pol u) = Cut loc (substTyVars varmap t) pol (substTyVars varmap u) 
  substTyVars _ (Done loc) = Done loc
  substTyVars _ (Err loc err) = Err loc err
  substTyVars varmap (Print loc t) = Print loc (substTyVars varmap t)

--------------------------------
-- Term Variable Substitution --
--------------------------------
class Subst a where 
  substVar :: a -> Term -> Variable -> a 

instance Subst Pattern where 
  substVar pt@(MkPattern xt args cmd) t v = 
    if v `elem` args then pt 
    else 
      let fv = freeVars t in 
      if any (`elem` args) fv then do
        let constrs = S.union fv (S.fromList args)
        let (_,freshVars) = foldr (\ov (c,vs) -> let newV = freshVar 0 c in (S.insert newV c,(newV,ov):vs) ) (constrs,[]) args
        let newc = foldr (\(nv,ov) c -> substVar c (Var defaultLoc nv (getType t)) ov) cmd freshVars
        MkPattern xt (fst <$> freshVars) (substVar newc t v)
    else MkPattern xt args (substVar cmd t v)

instance Subst Term where 
  substVar (Var loc v1 ty ) t v2 = if v1 == v2 then t else Var loc v1 ty
  substVar (Mu loc v1 cmd ty) t2 v2 =  
    if v1 == v2 then Mu loc v1 cmd ty
    else 
      let fv = freeVars t2
      in if v1 `elem` fv 
      then do
        let frV = freshVar 0 (S.union fv (freeVars cmd))
        let cmd' = substVar cmd (Var defaultLoc v1 ty) frV
        Mu loc frV (substVar cmd' t2 v2) ty
      else Mu loc v1 (substVar cmd t2 v2) ty
  substVar (Xtor loc xt args ty) t v = Xtor loc xt ((\x -> substVar x t v) <$> args) ty
  substVar (XCase loc pts ty) t v = XCase loc ((\x -> substVar x t v) <$> pts) ty
  substVar (ShiftPos loc t1 ty) t2 v = ShiftPos loc (substVar t1 t2 v) ty
  substVar (ShiftNeg loc v1 cmd ty) t v2 = 
    if v1 == v2 then ShiftNeg loc v1 cmd ty
    else 
      let fv = freeVars t 
      in if v1 `elem` fv 
      then do 
        let frV = freshVar 0 (S.union fv (freeVars cmd))
        let cmd' = substVar cmd (Var loc v1 ty) frV
        ShiftNeg loc frV (substVar cmd' t v2) ty
      else ShiftNeg loc v1 (substVar cmd t v2) ty

instance Subst Command where 
  substVar (Cut loc t1 pol t2) t3 v = Cut loc (substVar t1 t3 v) pol (substVar t2 t3 v)
  substVar (Done loc) _ _ = Done loc
  substVar (Err loc err) _ _ = Err loc err
  substVar (Print loc t) t2 v = Print loc (substVar t t2 v)
