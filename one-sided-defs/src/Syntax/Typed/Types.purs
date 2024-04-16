module Syntax.Typed.Types (
  Ty (..),
  KindedTy (..),
  isSubsumed ,
  embedType
) where 

import Prelude (class Eq, (==), (<$>), (<>), (&&), identity,map, class Show, show)
import Data.List (List, elem, null, filter, zip, intercalate)
import Data.Tuple (Tuple(..))

import Common (Typevar, Typename, Kind)
import Syntax.Desugared.Types (Ty(..)) as D

data Ty = 
  TyVar  Typevar 
  | TyDecl  Typename (List Ty)
  | TyShift  Ty 
  | TyCo  Ty 
  | TyForall  (List Typevar) Ty 
derive instance eqTy :: Eq Ty
instance Show Ty where
  show (TyVar v) = show v
  show (TyDecl tyn args) | null args = show tyn
  show (TyDecl tyn args) = show tyn <> " ( " <> intercalate ", " (show <$> args) <> ")"
  show (TyShift ty) = "{" <> show ty <> "}"
  show (TyCo ty) = "co " <> show ty 
  show (TyForall args ty) | null args = show ty
  show (TyForall args ty) = intercalate " " (show <$> args) <> ". " <> show ty

data KindedTy = KindedTy {kindedTy :: Ty, kindedKind :: Kind}
derive instance eqKindedTy :: Eq KindedTy
instance Show KindedTy where 
  show (KindedTy kty) = show kty.kindedTy <> ": " <> show kty.kindedKind

isSubsumed :: Ty -> Ty -> Boolean
isSubsumed ty1 ty2 | ty1 == ty2 = true
isSubsumed ty (TyForall args ty') = case ty' of 
  TyVar v -> v `elem` args
  TyDecl tyn tyargs-> isSubsumed ty (TyDecl tyn (TyForall args <$> tyargs))
  TyShift ty'' -> isSubsumed ty (TyShift (TyForall args ty''))
  TyCo ty'' -> isSubsumed ty (TyCo (TyForall args ty''))
  TyForall args' ty'' -> isSubsumed ty (TyForall (args<>args') ty'')
isSubsumed (TyVar _) (TyVar _) = true
isSubsumed (TyDecl tyn args) (TyDecl tyn' args') = tyn == tyn' && null (filter identity (map (\(Tuple x y) -> isSubsumed x y)  (zip args args')))
isSubsumed (TyShift ty) ty' = isSubsumed ty ty'
isSubsumed ty (TyShift ty') = isSubsumed ty ty'
isSubsumed (TyCo ty) (TyCo ty') = isSubsumed ty ty'
isSubsumed _ _ = false

embedType :: Ty -> D.Ty
embedType (TyVar v) = D.TyVar v
embedType (TyDecl tyn args) = D.TyDecl tyn (embedType <$> args)
embedType (TyShift ty) = D.TyShift (embedType ty)
embedType (TyCo ty) = D.TyCo (embedType ty)
embedType (TyForall args ty) = D.TyForall args (embedType ty)
