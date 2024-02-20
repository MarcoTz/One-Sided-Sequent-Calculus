module Eval.Substitution where 

import Syntax.Typed.Terms
import Common 

import Data.Set qualified as S


-- Free Variables 
class FreeVars a where 
  freeVars :: a -> S.Set Variable

instance FreeVars Term where 
  freeVars (Var v _)       = S.singleton v
  freeVars (Mu v c _)      = S.delete v (freeVars c)
  freeVars (Xtor _ args _) = S.unions (freeVars <$> args)
  freeVars (XCase pts _)   = S.unions (freeVars <$> pts)
  freeVars (Shift t _)     = freeVars t 
  freeVars (Lam v cmd _)   = S.delete v (freeVars cmd)

instance FreeVars Pattern where 
  freeVars MkPattern{ptxt=_, ptv=vars, ptcmd=st} = foldr S.delete (freeVars st) vars

instance FreeVars Command where 
  freeVars (Cut t1 _ t2) = S.union (freeVars t1) (freeVars t2) 
  freeVars Done = S.empty

freshVar :: Int -> S.Set Variable -> Variable 
freshVar n vars = let newV = "x"<> show n in if newV `elem` vars then freshVar (n+1) vars else newV


-- Substitution
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
