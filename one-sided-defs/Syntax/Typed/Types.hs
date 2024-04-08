module Syntax.Typed.Types (
  Ty (..),
  isSubsumed 
) where 

import Common 

data Ty where 
  TyVar :: Typevar -> Kind -> Ty 
  TyDecl :: Typename -> [Ty] -> Kind -> Ty 
  TyShift :: Ty -> Kind -> Ty
  TyCo :: Ty -> Ty 
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

isSubsumed :: Ty -> Ty -> Bool
isSubsumed ty1 ty2 | ty1 == ty2 = True
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


