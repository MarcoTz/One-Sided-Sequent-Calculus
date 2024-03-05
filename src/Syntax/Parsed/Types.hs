module Syntax.Parsed.Types where 

import Common


data Ty = 
  TyVar !TypeVar 
  | TyDecl !TypeName ![Ty]
  | TyCo !Ty
--  | TyForall ![PolVar] !Ty

--freeTyVars :: Ty -> S.Set TypeVar 
--freeTyVars (TyVar v) = S.singleton v
--freeTyVars (TyDecl _ args) = S.unions (freeTyVars <$> args)
--freeTyVars (TyForall vars ty) = S.difference (freeTyVars ty) (S.fromList vars)

--generalize :: Ty -> TypeScheme
--generalize ty = MkTypeScheme (S.toList $ freeTyVars ty) ty
