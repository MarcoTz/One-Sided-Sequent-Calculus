module Syntax.Parsed.Types (
  KindedTy (..),
  Ty (..)
) where 

import Common (Typevar,Typename,Kind)

import Prelude (class Eq, class Ord, class Show, show, (<>), (<$>))
import Data.List (List,intercalate, null)

data Ty =
  TyVar     Typevar 
  |TyDecl   Typename (List Ty) 
  |TyCo     Ty 
  |TyShift  Ty 
  |TyForall (List Typevar) Ty 
derive instance eqTy :: Eq Ty 
derive instance ordTy :: Ord Ty 
instance Show Ty where 
  show (TyVar v) = show v 
  show (TyDecl nm args) | null args = show nm
  show (TyDecl nm args) = show nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (TyCo ty) = "co " <> show ty
  show (TyShift ty) = "{" <> show ty <> "}"
  show (TyForall args ty) = "forall " <> intercalate ", " (show <$> args) <> ". " <> show ty

data KindedTy = KindedTy { kindedTy :: Ty, kindedKind :: Kind }
derive instance eqKindedTy :: Eq KindedTy 
derive instance ordKindedTy :: Ord KindedTy 
instance Show KindedTy where 
  show (KindedTy kty) = show kty.kindedTy<> " : " <> show kty.kindedKind
