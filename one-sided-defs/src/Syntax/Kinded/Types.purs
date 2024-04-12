module Syntax.Kinded.Types (
  Ty (..)
) where 

import Common (Typevar,Kind, Typename, class GetKind, getKind, shiftEvalOrder, class ContainsKindvar, containsKindvar, class ShiftEvalOrder)

import Prelude (class Eq, (<$>), ($), class Show, show, (<>))
import Data.List (List,null,intercalate)

data Ty =
  TyVar      Typevar Kind 
  | TyDecl   Typename (List Ty) Kind 
  | TyShift  Ty Kind 
  | TyCo     Ty 
  | TyForall (List Typevar)  Ty
derive instance eqTy :: Eq Ty
instance Show Ty where 
  show (TyVar v knd) = show v <> " : " <> show knd 
  show (TyDecl tyn args knd) | null args = show tyn <> ": " <> show knd
  show (TyDecl tyn args knd) = show tyn <> "(" <> intercalate ", " (show <$> args) <> ") :" <> show knd
  show (TyShift ty knd) = "{" <> show ty <> "}" <> ":" <> show knd
  show (TyCo ty) = "co " <> show ty
  show (TyForall args ty) | null args = show ty
  show (TyForall args ty) = "forall " <> intercalate ", " (show <$> args) <> ", " <> show ty


instance GetKind Ty where 
  getKind (TyVar _ knd)     = knd
  getKind (TyDecl _ _ knd)  = knd
  getKind (TyShift _ knd)   = knd
  getKind (TyCo ty)         = shiftEvalOrder (getKind ty)
  getKind (TyForall _ ty)   = getKind ty

instance ContainsKindvar Ty where 
  containsKindvar ty = containsKindvar (getKind  $ ty)

instance ShiftEvalOrder Ty where 
  shiftEvalOrder (TyVar v knd) = TyVar v (shiftEvalOrder knd)
  shiftEvalOrder (TyDecl tyn tyargs knd) = TyDecl tyn (shiftEvalOrder <$> tyargs) (shiftEvalOrder knd)
  shiftEvalOrder (TyShift ty knd) = TyShift (shiftEvalOrder ty) (shiftEvalOrder knd)
  shiftEvalOrder (TyCo ty) = TyCo (shiftEvalOrder ty)
  shiftEvalOrder (TyForall args ty) = TyForall args (shiftEvalOrder ty)
