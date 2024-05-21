module Syntax.Kinded.Types (
  Ty (..),
  embedType
) where 

import Common (Typevar,Kind, Typename(..), shiftEvalOrder,
  class GetKind, getKind, class ShiftEvalOrder)
import Syntax.Typed.Types (Ty(..)) as T

import Prelude (class Eq, (<$>), class Show, show, (<>))
import Data.List (List(..),null,intercalate)

data Ty =
  TyVar      Typevar Kind 
  | TyDecl   Typename (List Ty) Kind 
  | TyShift  Ty Kind 
  | TyCo     Ty 
  | TyForall (List Typevar)  Ty
derive instance eqTy :: Eq Ty
instance Show Ty where 
  show (TyVar v knd) = "(" <> show v <> " : " <> show knd <> ")"
  show (TyDecl (Typename "Fun") (Cons ty1 (Cons ty2 Nil)) knd) = "(" <> show ty1 <> " -> " <> show ty2 <> " : " <> show knd <> ")"
  show (TyDecl tyn args knd) | null args = "(" <> show tyn <> ": " <> show knd <> ")"
  show (TyDecl tyn args knd) = "(" <> show tyn <> "(" <> intercalate ", " (show <$> args) <> ") :" <> show knd <> ")"
  show (TyShift ty knd) = "({" <> show ty <> "}" <> ":" <> show knd <> ")"
  show (TyCo ty) = "(co " <> show ty<>")"
  show (TyForall args ty) | null args = show ty
  show (TyForall args ty) = "(forall " <> intercalate ", " (show <$> args) <> ", " <> show ty <>")"


instance GetKind Ty where 
  getKind (TyVar _ knd)     = knd
  getKind (TyDecl _ _ knd)  = knd
  getKind (TyShift _ knd)   = knd
  getKind (TyCo ty)         = shiftEvalOrder (getKind ty)
  getKind (TyForall _ ty)   = getKind ty


instance ShiftEvalOrder Ty where 
  shiftEvalOrder (TyVar v knd) = TyVar v (shiftEvalOrder knd)
  shiftEvalOrder (TyDecl tyn tyargs knd) = TyDecl tyn (shiftEvalOrder <$> tyargs) (shiftEvalOrder knd)
  shiftEvalOrder (TyShift ty knd) = TyShift (shiftEvalOrder ty) (shiftEvalOrder knd)
  shiftEvalOrder (TyCo ty) = TyCo (shiftEvalOrder ty)
  shiftEvalOrder (TyForall args ty) = TyForall args (shiftEvalOrder ty)

embedType :: Ty -> T.Ty
embedType (TyVar v _) = T.TyVar v
embedType (TyDecl tyn tyargs _) = T.TyDecl tyn (embedType <$> tyargs)
embedType (TyShift ty _) = T.TyShift (embedType ty)
embedType (TyCo ty) = T.TyCo (embedType ty)
embedType (TyForall args ty) = T.TyForall args (embedType ty)
