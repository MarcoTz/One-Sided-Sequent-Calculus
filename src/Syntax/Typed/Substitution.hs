module Syntax.Typed.Substitution where 

import Syntax.Typed.Types 
import Syntax.Typed.Program
import Syntax.Typed.Terms
import Syntax.Typed.FreeVars
import Common

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
  substVars :: M.Map Variable Ty -> a -> a 

instance SubstTyVars XtorSig where 
  substVars varmap (MkXtorSig nm args) = MkXtorSig nm (substVars varmap <$> args)

instance SubstTyVars Ty where 
  substVars varmap ty@(TyVar v _) = fromMaybe ty (M.lookup v varmap) 
  substVars varmap (TyDecl tyn args knd) = TyDecl tyn (substVars varmap <$> args) knd
  substVars varmap (TyShift ty knd) = TyShift (substVars varmap ty) knd
  substVars varmap (TyCo ty knd) = TyCo (substVars varmap ty) knd

instance SubstTyVars Term where 
  substVars varmap (Var v ty) = Var v (substVars varmap ty)
  substVars varmap (Mu v c ty) = Mu v (substVars varmap c) (substVars varmap ty)
  substVars varmap (Xtor nm args ty) = Xtor nm (substVars varmap <$> args) (substVars varmap ty)
  substVars varmap (XCase pts ty) = XCase (substVars varmap <$> pts) (substVars varmap ty)
  substVars varmap (Shift t ty) = Shift (substVars varmap t) (substVars varmap ty)
  substVars varmap (Lam v t ty) = Lam v (substVars varmap t) (substVars varmap ty)

instance SubstTyVars Pattern where 
  substVars varmap (MkPattern xt vars c) = MkPattern xt vars (substVars varmap c)

instance SubstTyVars Command where 
  substVars varmap (Cut t pol u) = Cut (substVars varmap t) pol (substVars varmap u) 
  substVars _ Done = Done

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
        let newc = foldr (\(nv,ov) c -> substVar c (Var nv (getType t)) ov) cmd freshVars
        MkPattern xt (fst <$> freshVars) (substVar newc t v)
    else MkPattern xt args (substVar cmd t v)

instance Subst Term where 
  substVar (Var v1 ty ) t v2 = if v1 == v2 then t else Var v1 ty
  substVar (Mu v1 cmd ty) t2 v2 =  
    if v1 == v2 then Mu v1 cmd ty
    else 
      let fv = freeVars t2
      in if v1 `elem` fv 
      then do
        let frV = freshVar 0 (S.union fv (freeVars cmd))
        let cmd' = substVar cmd (Var v1 ty) frV
        Mu frV (substVar cmd' t2 v2) ty
      else Mu v1 (substVar cmd t2 v2) ty
  substVar (Xtor xt args ty) t v = Xtor xt ((\x -> substVar x t v) <$> args) ty
  substVar (XCase pts ty) t v = XCase ((\x -> substVar x t v) <$> pts) ty
  substVar (Shift t1 ty) t2 v = Shift (substVar t1 t2 v) ty
  substVar (Lam v1 cmd ty) t v2 = 
    if v1 == v2 then Lam v1 cmd ty
    else 
      let fv = freeVars t 
      in if v1 `elem` fv 
      then do 
        let frV = freshVar 0 (S.union fv (freeVars cmd))
        let cmd' = substVar cmd (Var v1 ty) frV
        Lam frV (substVar cmd' t v2) ty
      else Lam v1 (substVar cmd t v2) ty

instance Subst Command where 
  substVar (Cut t1 pol t2) t3 v = Cut (substVar t1 t3 v) pol (substVar t2 t3 v)
  substVar Done _ _ = Done
