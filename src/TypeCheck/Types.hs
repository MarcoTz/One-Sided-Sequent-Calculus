module TypeCheck.Types where 

import TypeCheck.Definition
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Program   qualified as T
import Errors
import Common
import Environment

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

checkType :: D.Ty -> Pol -> CheckM T.Ty
checkType (D.TyVar v) pol = do
  tyVars <- gets checkTyVars
  if v `elem` tyVars then return $ T.TyVar v pol else throwError (ErrMissingTyVar v "checkType TyVar")

checkType (D.TyDecl tyn args) pol = do 
   T.MkData _ argVars _  _ <- lookupDecl tyn
   polPairs <- zipWithError args (getKind <$> argVars) (ErrTyArity tyn " checkType TyDecl")
   args' <- forM polPairs (uncurry checkType)
   return $ T.TyDecl tyn args' pol 

checkType (D.TyCo ty) pol = T.TyCo <$> checkType ty (flipPol pol)
checkType (D.TyForall args ty) pol = do
  forM_ args addTyVar 
  T.TyForall args <$> checkType ty pol

checkPolTy :: D.PolTy -> CheckM T.Ty
checkPolTy (D.MkPolTy (D.TyVar v) pol) = return $ T.TyVar v pol
checkPolTy (D.MkPolTy (D.TyDecl tyn tyargs) pol) = do 
  T.MkData _ tyargs'  _ _ <- lookupDecl tyn
  tyArgsZipped <- zipWithError tyargs (getKind <$> tyargs') (ErrTyArity tyn "checkPolTy")
  args' <- forM tyArgsZipped (uncurry checkType)
  return $ T.TyDecl tyn args' pol
checkPolTy (D.MkPolTy (D.TyCo ty) pol) = T.TyCo <$> checkPolTy (D.MkPolTy ty (flipPol pol))
checkPolTy (D.MkPolTy (D.TyForall args ty) pol) = do
  forM_ args addTyVar
  T.TyForall args <$> checkPolTy (D.MkPolTy ty pol)
