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
