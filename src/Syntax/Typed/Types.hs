module Syntax.Typed.Types where 

import Common 

import Data.Set qualified as S

data TypeScheme = MkTypeScheme ![TypeVar] !Ty
data Ty = 
  TyVar !TypeVar !Pol
  | TyDecl !TypeName ![Ty] !Pol
  | TyShift !Ty !Pol
  | TyCo !Ty !Pol
  deriving (Eq)


freeTyVars :: Ty -> S.Set TypeVar
freeTyVars (TyVar v _) = S.singleton v 
freeTyVars (TyDecl _ args _) = S.unions (freeTyVars <$> args)
freeTyVars (TyShift ty _) = freeTyVars ty
freeTyVars (TyCo ty _) = freeTyVars ty

generalize :: Ty -> TypeScheme
generalize ty = let vars = freeTyVars ty in MkTypeScheme (S.toList vars) ty 

instance GetKind Ty where 
  getKind (TyVar _ knd) = knd
  getKind (TyDecl _ _ knd) = knd
  getKind (TyShift _ knd) = knd
  getKind (TyCo _ knd) = knd

flipPolTy :: Ty -> Ty 
flipPolTy (TyVar v pol) = TyVar v (flipPol pol)
flipPolTy (TyDecl tyn args pol) = TyDecl tyn (flipPolTy <$> args) (flipPol pol)
flipPolTy (TyShift ty pol) = TyShift (flipPolTy ty) (flipPol pol)
flipPolTy (TyCo ty pol) = TyCo (flipPolTy ty) (flipPol pol)

isSubsumed :: Ty -> TypeScheme -> Bool
isSubsumed (TyDecl tyn args pol) (MkTypeScheme tyvars (TyDecl tyn' args' pol')) = 
  let newTys = MkTypeScheme tyvars <$> args' in 
    tyn == tyn' && pol==pol' && all (uncurry isSubsumed) (zip args newTys)
isSubsumed (TyShift ty pol) (MkTypeScheme tyvars (TyShift ty' pol')) = pol == pol' && isSubsumed ty (MkTypeScheme tyvars ty')
isSubsumed (TyCo ty pol) (MkTypeScheme tyvars (TyCo ty' pol')) = pol==pol' && isSubsumed ty (MkTypeScheme tyvars ty')
isSubsumed ty (MkTypeScheme tyvars (TyVar v pol)) = v `elem` tyvars && getKind ty == pol
isSubsumed _ _ = False

