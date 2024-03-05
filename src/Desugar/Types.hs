module Desugar.Types where

import Common
import Errors
import Desugar.Definition
import Syntax.Parsed.Types    qualified as P
import Syntax.Parsed.Program  qualified as P
import Syntax.Desugared.Types qualified as D

import Control.Monad
import Control.Monad.Except
import Data.List (find)

desugarTy :: P.Ty -> DesugarM D.Ty
desugarTy (P.TyVar v) = do 
  let vty = tyvarToTyName v
  mdecl <- getMDecl vty
  case mdecl of 
    Nothing -> do 
      currDecl <- getCurrDecl (ErrMissingDecl vty "desugarTy")
      case find (\(MkPolVar v' _) -> v'==v) (P.declArgs currDecl) of 
        Nothing -> throwError (ErrMissingDecl vty "desugarTy")
        Just _ -> return $ D.TyVar v 
    Just _ -> return $ D.TyDecl vty [] 

desugarTy (P.TyDecl tyn args) = do 
  args' <- forM args desugarTy 
  return $ D.TyDecl tyn args' 

desugarTy (P.TyCo ty) = D.TyCo <$> desugarTy ty


--desugarTy (P.TyForall vars ty) = do 
--  ty' <- desugarTy ty
--  return $ D.TyForall vars ty' 
