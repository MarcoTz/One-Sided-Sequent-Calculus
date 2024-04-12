module Syntax.Typed.Types (
  Ty (..),
  KindedTy (..),
  isSubsumed 
) where 

import Common 

data Ty where 
  TyVar :: Typevar -> Ty 
  TyDecl :: Typename -> [Ty] -> Ty 
  TyShift :: Ty -> Ty
  TyCo :: Ty -> Ty 
  TyForall :: [Typevar] -> Ty -> Ty
  deriving (Eq)

data KindedTy = KindedTy {kindedTy :: !Ty, kindedKind :: !Kind}
  deriving (Eq)

isSubsumed :: Ty -> Ty -> Bool
isSubsumed ty1 ty2 | ty1 == ty2 = True
isSubsumed ty (TyForall args ty') = case ty' of 
  TyVar v -> v `elem` args
  TyDecl tyn tyargs-> isSubsumed ty (TyDecl tyn (TyForall args <$> tyargs))
  TyShift ty'' -> isSubsumed ty (TyShift (TyForall args ty''))
  TyCo ty'' -> isSubsumed ty (TyCo (TyForall args ty''))
  TyForall args' ty'' -> isSubsumed ty (TyForall (args++args') ty'')
isSubsumed (TyVar _) (TyVar _) = True
isSubsumed (TyDecl tyn args) (TyDecl tyn' args') = tyn == tyn' && all (uncurry isSubsumed)  (zip args args')
isSubsumed (TyShift ty) ty' = isSubsumed ty ty'
isSubsumed ty (TyShift ty') = isSubsumed ty ty'
isSubsumed (TyCo ty) (TyCo ty') = isSubsumed ty ty'
isSubsumed _ _ = False


