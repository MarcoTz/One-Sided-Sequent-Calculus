module Syntax.Parsed.Types where 

import Common

import Data.Set qualified as S

data TypeScheme = MkTypeScheme ![TypeVar] !Ty

data Ty = TyVar !TypeVar | TyDecl !TypeName ![Ty]

freeTyVars :: Ty -> S.Set TypeVar 
freeTyVars (TyVar v) = S.singleton v
freeTyVars (TyDecl _ args) = S.unions (freeTyVars <$> args)

generalize :: Ty -> TypeScheme
generalize ty = MkTypeScheme (S.toList $ freeTyVars ty) ty
