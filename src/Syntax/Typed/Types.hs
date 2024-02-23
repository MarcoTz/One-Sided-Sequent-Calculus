module Syntax.Typed.Types where 

import Common 

import Data.Set qualified as S

data TypeScheme = MkTypeScheme ![TypeVar] !Ty
data Ty = 
  TyVar !TypeVar 
  | TyDecl !TypeName ![Ty]
  | TyShift !Ty
  | TyCo !Ty 
  deriving (Eq)


getTyVars :: Ty -> S.Set TypeVar
getTyVars (TyVar v) = S.singleton v
getTyVars (TyDecl _ args) = S.unions (getTyVars <$> args)
getTyVars (TyShift ty) = getTyVars ty
getTyVars (TyCo ty) = getTyVars ty

generalize :: Ty -> TypeScheme
generalize ty = let vars = getTyVars ty in MkTypeScheme (S.toList vars) ty 
