module Syntax.Typed.Types where 

import Common 

data Ty = 
  TyVar !TypeVar !Pol
  | TyDecl !TypeName ![Ty] !Pol
  | TyShift !Ty !Pol
  | TyCo !Ty
  | TyForall ![TypeVar] !Ty
  deriving (Eq)

instance GetKind Ty where 
  getKind (TyVar _ knd)     = knd
  getKind (TyDecl _ _ knd)  = knd
  getKind (TyShift _ knd)   = knd
  getKind (TyCo ty)         = flipPol (getKind ty)
  getKind (TyForall _ ty)   = getKind ty

instance FlipPol Ty where 
  flipPol (TyVar v knd) = TyVar v (flipPol knd)
  flipPol (TyDecl tyn args knd) = TyDecl tyn args (flipPol knd)
  flipPol (TyShift ty knd) = TyShift ty (flipPol knd)
  flipPol (TyCo ty) = TyCo (flipPol ty)
  flipPol (TyForall args ty) = TyForall args (flipPol ty)

isSubsumed :: Ty -> Ty -> Bool
isSubsumed ty (TyForall args ty') = case ty' of 
  TyVar v _ -> v `elem` args
  TyDecl tyn tyargs knd -> isSubsumed ty (TyDecl tyn (TyForall args <$> tyargs) knd)
  TyShift ty'' knd -> isSubsumed ty (TyShift (TyForall args ty'') knd)
  TyCo ty'' -> isSubsumed ty (TyCo (TyForall args ty''))
  TyForall args' ty'' -> isSubsumed ty (TyForall (args++args') ty'')
isSubsumed (TyVar _ _) (TyVar _ _) = True
isSubsumed (TyDecl tyn args _) (TyDecl tyn' args' _) = tyn == tyn' && all (uncurry isSubsumed)  (zip args args')
isSubsumed (TyShift ty _) ty' = isSubsumed ty ty'
isSubsumed ty (TyShift ty' _) = isSubsumed ty ty'
isSubsumed (TyCo ty) (TyCo ty') = isSubsumed ty ty'
isSubsumed _ _ = False

