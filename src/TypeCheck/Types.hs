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
import Data.Map qualified as M


checkType :: D.Ty -> Pol -> CheckM T.Ty
checkType (D.TyVar v) pol = do
  tyVars <- gets checkTyVars
  case M.lookup v tyVars of 
    Nothing -> throwError (ErrMissingTyVar v "checkType TyVar")
    Just pol' -> if pol' == pol then return (T.TyVar v pol) else  throwError (ErrKind ShouldEq "checkType TyVar")

checkType (D.TyDecl tyn args) pol = do 
   T.MkData _ argVars _  _ <- lookupDecl tyn
   polPairs <- zipWithError args (getKind <$> argVars) (ErrTyArity tyn " checkType TyDecl")
   args' <- forM polPairs (uncurry checkType)
   return $ T.TyDecl tyn args' pol 

checkType (D.TyCo ty) pol = T.TyCo <$> checkType ty (flipPol pol)

checkPolTy :: D.PolTy -> CheckM T.Ty
checkPolTy (D.TyVar v,pol) = return $ T.TyVar v pol
checkPolTy (D.TyDecl tyn tyargs,pol) = do 
  T.MkData _ tyargs'  _ _<- lookupDecl tyn
  tyArgsZipped <- zipWithError tyargs (getKind <$> tyargs') (ErrTyArity tyn "checkPolTy")
  args' <- forM tyArgsZipped (uncurry checkType)
  return $ T.TyDecl tyn args' pol
checkPolTy (D.TyCo ty, pol) = T.TyCo <$> checkPolTy (ty, flipPol pol)
