module Desugar.Types (
  desugarKindedTy,
  desugarTy
) where

import Environment (getTypeNames)
import Desugar.Definition(DesugarM,tyvarToTyName,getDesDefNames)
import Syntax.Parsed.Types (Ty(..),KindedTy(..)) as P 
import Syntax.Desugared.Types (Ty(..),KindedTy(..)) as D 

import Prelude (bind,pure,(<>),($),(<$>))
import Data.Traversable (for)
import Data.List (List(..),elem)

desugarTy :: P.Ty -> DesugarM D.Ty
desugarTy (P.TyVar v) = do 
  let vty = tyvarToTyName v
  declTys <- getTypeNames
  currNames <- getDesDefNames
  if vty `elem` (declTys<>currNames) then pure (D.TyDecl vty Nil) else pure $ D.TyVar v 
desugarTy (P.TyDecl tyn args) = do 
  args' <- for args desugarTy 
  pure $ D.TyDecl tyn args' 
desugarTy (P.TyCo ty) = D.TyCo <$> desugarTy ty
desugarTy (P.TyShift ty) = D.TyShift <$> desugarTy ty
desugarTy (P.TyForall args ty) = D.TyForall args <$> desugarTy ty

desugarKindedTy :: P.KindedTy -> DesugarM D.KindedTy
desugarKindedTy (P.KindedTy kty) = do
  ty' <- desugarTy kty.kindedTy
  pure (D.KindedTy {kindedTy:ty',kindedKind:kty.kindedKind})
