module Syntax.Typed.Types where 

import Common 

data Ty = 
  TyVar !TypeVar !Pol
  | TyDecl !TypeName ![Ty] !Pol
  | TyShift !Ty 
  | TyCo !Ty
  | TyForall ![TypeVar] !Ty
  deriving (Eq)

instance GetKind Ty where 
  getKind (TyVar _ knd)     = knd
  getKind (TyDecl _ _ knd)  = knd
  getKind (TyShift _)       = Pos
  getKind (TyCo ty)         = flipPol (getKind ty)
  getKind (TyForall _ ty)   = getKind ty

instance FlipPol Ty where 
  flipPol (TyVar v knd) = TyVar v (flipPol knd)
  flipPol (TyDecl tyn args knd) = TyDecl tyn (flipPol <$> args) (flipPol knd)
  flipPol (TyShift _) = error "Cannot flip polarity of shift"
  flipPol (TyCo ty) = TyCo (flipPol ty)
  flipPol (TyForall args ty) = TyForall args (flipPol ty)

isSubsumed :: Ty -> Ty -> Bool
isSubsumed ty (TyForall args ty') = case ty' of 
  TyVar v _ -> v `elem` args
  TyDecl tyn tyargs knd -> isSubsumed ty (TyDecl tyn (TyForall args <$> tyargs) knd)
  TyShift ty'' -> isSubsumed ty (TyShift (TyForall args ty''))
  TyCo ty'' -> isSubsumed ty (TyCo (TyForall args ty''))
  TyForall args' ty'' -> isSubsumed ty (TyForall (args++args') ty'')
isSubsumed (TyVar _ _) (TyVar _ _) = True
isSubsumed (TyDecl tyn args _) (TyDecl tyn' args' _) = tyn == tyn' && all (uncurry isSubsumed)  (zip args args')
isSubsumed (TyShift ty) ty' = isSubsumed ty ty'
isSubsumed ty (TyShift ty') = isSubsumed ty ty'
isSubsumed (TyCo ty) (TyCo ty') = isSubsumed ty ty'
isSubsumed _ _ = False

