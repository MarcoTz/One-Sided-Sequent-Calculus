module Syntax.Desugared.Types (
  KindedTy (..),
  Ty (..)
) where 

import Common (Typevar,Typename(..), Kind, class ShiftEvalOrder, shiftEvalOrder)

import Prelude (class Eq, class Show, show, (<>), (<$>))
import Data.List (List(..), intercalate, null)

data Ty = 
  TyVar      Typevar
  | TyDecl   Typename (List Ty)
  | TyCo     Ty
  | TyShift  Ty 
  | TyForall (List Typevar) Ty 
derive instance eqTy :: Eq Ty 

instance Show Ty where 
  show (TyVar v) = show v
  show (TyDecl (Typename "Fun") (Cons ty1 (Cons ty2 Nil))) = show ty1 <> " -> " <> show ty2
  show (TyDecl tyn args) | null args = show tyn
  show (TyDecl tyn args) = show tyn <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (TyCo ty) = "co " <> show ty
  show (TyShift ty) = "{ " <> show ty <> " }"
  show (TyForall args ty) = "forall " <> intercalate " " (show <$> args) <> ". " <> show ty

data KindedTy = KindedTy {kindedTy :: Ty, kindedKind :: Kind}
derive instance eqKindedTy :: Eq KindedTy
instance Show KindedTy where 
  show (KindedTy kty) = show kty.kindedTy <> " : " <> show kty.kindedKind

instance ShiftEvalOrder KindedTy where 
  shiftEvalOrder (KindedTy kty) = KindedTy (kty {kindedKind=shiftEvalOrder kty.kindedKind})
