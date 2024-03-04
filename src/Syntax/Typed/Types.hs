module Syntax.Typed.Types where 

import Common 

data Ty = 
  TyVar !TypeVar !Pol
  | TyDecl !TypeName ![Ty] !Pol
  | TyShift !Ty 
  | TyCo !Ty
--  | TyForall ![TypeVar] !Ty
  deriving (Eq)

instance GetKind Ty where 
  getKind (TyVar _ knd) = knd
  getKind (TyDecl _ _ knd) = knd
  getKind (TyShift _) = Pos
  getKind (TyCo ty) = flipPol (getKind ty)
--  getKind (TyForall _ ty) = getKind ty

--freeTyVars :: Ty -> S.Set TypeVar
--freeTyVars (TyVar v _) = S.singleton v 
--freeTyVars (TyDecl _ args _) = S.unions (freeTyVars <$> args)
--freeTyVars (TyShift ty) = freeTyVars ty
--freeTyVars (TyCo ty) = freeTyVars ty
--freeTyVars (TyForall vars ty) = S.difference (freeTyVars ty) (S.fromList vars) 

--generalize :: Ty -> Ty
--generalize ty = let vars = freeTyVars ty in TyForall (S.toList vars) ty 

--isSubsumed :: Ty -> Ty -> Bool
--isSubsumed _ (TyForall vars (TyVar v _)) = v `elem` vars
--isSubsumed (TyVar _ pol1) (TyVar _ pol2) = pol1 == pol2
--isSubsumed (TyVar v pol1) (TyForall vars ty) = pol1 == getKind ty && v `elem` vars
--isSubsumed (TyDecl tyn1 args1 pol1) (TyDecl tyn2 args2 pol2) = tyn1 == tyn2 && pol1 == pol2 && all (uncurry isSubsumed) (zip args1 args2) 
--isSubsumed (TyDecl tyn1 args1 pol1) (TyForall vars (TyDecl tyn2 args2 pol2)) = let genTys = TyForall vars <$> args2 in pol1 == pol2 && tyn1 == tyn2 && all (uncurry isSubsumed) (zip args1 genTys)
--isSubsumed (TyShift ty1) (TyShift ty2) = isSubsumed ty1 ty2
--isSubsumed (TyShift ty1) (TyForall vars (TyShift ty2)) = isSubsumed ty1 (TyForall vars ty2)
--isSubsumed (TyCo ty1) (TyCo ty2) = isSubsumed ty1 ty2
--isSubsumed (TyCo ty1) (TyForall vars (TyCo ty2)) = isSubsumed ty1 (TyForall vars ty2)
--isSubsumed (TyForall _ ty1) (TyForall _ ty2) = isSubsumed ty1 ty2
--isSubsumed _ _ = False 
