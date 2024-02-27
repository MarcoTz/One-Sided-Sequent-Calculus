module Syntax.Typed.Types where 

import Common 

import Data.Set qualified as S

data TypeScheme = MkTypeScheme ![TypeVar] !Ty
data Ty = 
  TyVar !TypeVar !Kind
  | TyDecl !TypeName ![Ty] !Kind
  | TyShift !Ty !Kind
  | TyCo !Ty !Kind
  deriving (Eq)


getTyVars :: Ty -> S.Set TypeVar
getTyVars (TyVar v _) = S.singleton v
getTyVars (TyDecl _ args _) = S.unions (getTyVars <$> args)
getTyVars (TyShift ty _) = getTyVars ty
getTyVars (TyCo ty _) = getTyVars ty

generalize :: Ty -> TypeScheme
generalize ty = let vars = getTyVars ty in MkTypeScheme (S.toList vars) ty 

instance GetKind Ty where 
  getKind (TyVar _ knd) = knd
  getKind (TyDecl _ _ knd) = knd
  getKind (TyShift _ knd) = knd
  getKind (TyCo _ knd) = knd
