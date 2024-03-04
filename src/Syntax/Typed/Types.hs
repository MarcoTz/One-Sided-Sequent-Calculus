module Syntax.Typed.Types where 

import Common 

import Data.Set qualified as S

data Ty = 
  TyVar !TypeVar !Pol
  | TyDecl !TypeName ![Ty] !Pol
  | TyShift !Ty 
  | TyCo !Ty
  | TyForall ![TypeVar] !Ty
  deriving (Eq)

instance GetKind Ty where 
  getKind (TyVar _ knd) = knd
  getKind (TyDecl _ _ knd) = knd
  getKind (TyShift _) = Pos
  getKind (TyCo ty) = flipPol (getKind ty)
  getKind (TyForall _ ty) = getKind ty

freeTyVars :: Ty -> S.Set TypeVar
freeTyVars (TyVar v _) = S.singleton v 
freeTyVars (TyDecl _ args _) = S.unions (freeTyVars <$> args)
freeTyVars (TyShift ty) = freeTyVars ty
freeTyVars (TyCo ty) = freeTyVars ty
freeTyVars (TyForall vars ty) = S.difference (freeTyVars ty) (S.fromList vars) 

generalize :: Ty -> Ty
generalize ty = let vars = freeTyVars ty in TyForall (S.toList vars) ty 


flipPolTy :: Ty -> Ty 
flipPolTy _ = error "not implemented (flipPolTy)"

isSubsumed :: Ty -> Ty -> Bool
isSubsumed _ _ = error "not implemented (isSubsumed)"
