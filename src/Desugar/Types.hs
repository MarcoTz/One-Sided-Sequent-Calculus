module Desugar.Types where

import Common
import Errors
import Desugar.Definition
import Environment qualified as Env
import Syntax.Parsed.Types    qualified as P
import Syntax.Parsed.Terms    qualified as P
import Syntax.Parsed.Program  qualified as P
import Syntax.Desugared.Types qualified as D
import Syntax.Desugared.Terms qualified as D

import Control.Monad
import Control.Monad.Except
import Data.List (find)

desugarTy :: P.Ty -> DesugarM D.Ty
desugarTy (P.TyVar v) = do 
  let vty = tyvarToTyName v
  declTys <- Env.getTypeNames
  currNames <- getDefNames
  if vty `elem` declTys++currNames then return (D.TyDecl vty []) else do
    currDecl <- getCurrDecl (ErrMissingDecl vty "desugarTy")
    case find (\(MkPolVar v' _) -> v'==v) (P.declArgs currDecl) of 
      Nothing -> throwError (ErrMissingDecl vty "desugarTy")
      Just _ -> return $ D.TyVar v 
desugarTy (P.TyDecl tyn args) = do 
  args' <- forM args desugarTy 
  return $ D.TyDecl tyn args' 
desugarTy (P.TyCo ty) = D.TyCo <$> desugarTy ty

desugarPolTy :: P.PolTy -> DesugarM D.PolTy
desugarPolTy (ty,pol) = (,pol) <$> desugarTy ty

desugarVarTy :: P.MTypedVar -> DesugarM D.MTypedVar 
desugarVarTy (v,Nothing) = return (v,Nothing) 
desugarVarTy (v, Just ty) = do 
  ty' <- desugarPolTy ty 
  return (v,Just ty')
