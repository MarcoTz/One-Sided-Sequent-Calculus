module Syntax.Kinded.Types (
  Ty (..),
) where 

import Common 

data Ty where 
  TyVar    :: Typevar -> Kind -> Ty 
  TyDecl   :: Typename -> [Ty] -> Kind -> Ty 
  TyShift  :: Ty -> Kind -> Ty
  TyCo     :: Ty -> Ty 
  TyForall :: [Typevar] -> Ty -> Ty
  deriving (Eq)

instance GetKind Ty where 
  getKind (TyVar _ knd)     = knd
  getKind (TyDecl _ _ knd)  = knd
  getKind (TyShift _ knd)   = knd
  getKind (TyCo ty)         = shiftEvalOrder (getKind ty)
  getKind (TyForall _ ty)   = getKind ty

instance ContainsKindvar Ty where 
  containsKindvar = containsKindvar . getKind

instance ShiftEvalOrder Ty where 
  shiftEvalOrder (TyVar v knd) = TyVar v (shiftEvalOrder knd)
  shiftEvalOrder (TyDecl tyn tyargs knd) = TyDecl tyn (shiftEvalOrder <$> tyargs) (shiftEvalOrder knd)
  shiftEvalOrder (TyShift ty knd) = TyShift (shiftEvalOrder ty) (shiftEvalOrder knd)
  shiftEvalOrder (TyCo ty) = TyCo (shiftEvalOrder ty)
  shiftEvalOrder (TyForall args ty) = TyForall args (shiftEvalOrder ty)
