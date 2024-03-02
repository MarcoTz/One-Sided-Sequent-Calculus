module Syntax.Typed.Types where 

import Common 

import Data.Set qualified as S

data TypeScheme = MkTypeScheme ![PolVar] !Ty
data Ty = 
  TyVar !TypeVar !Pol
  | TyDecl !TypeName ![Ty] !Pol
  | TyShift !Ty !Pol
  | TyCo !Ty !Pol
  deriving (Eq)


freeTyVars :: Ty -> S.Set PolVar 
freeTyVars (TyVar v pol) = S.singleton (MkPolVar v pol)
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
