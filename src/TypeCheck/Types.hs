module TypeCheck.Types where 

import TypeCheck.Definition
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Program   qualified as T
import Common
import Errors
import Environment

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as M



checkType :: D.Ty -> Pol -> CheckM T.Ty
checkType (D.TyVar v) pol = do
  tyVars <- gets checkTyVars
  forallVars <- gets checkForall 
  if v `elem` forallVars then return $ T.TyVar v pol else
    case M.lookup v tyVars of 
      Nothing -> throwError (ErrMissingTyVar v WhereCheck)
      Just pol' -> if pol == pol' then return (T.TyVar v pol) else throwError (ErrKind ShouldEq WhereCheck)

checkType (D.TyDecl tyn args) pol = do 
   T.MkDataDecl _ args' pol' _ <- lookupDecl tyn
   unless (pol == pol') $ throwError (ErrKind ShouldEq WhereCheck)
   argsZipped <- zipWithError args (getKind <$> args') (ErrTyArity tyn WhereCheck)
   args'' <- forM argsZipped (uncurry checkType)
   return $ T.TyDecl tyn args'' pol
checkType (D.TyForall tyvars ty) pol = do
  setForall tyvars
  ty' <- checkType ty pol 
  return $ T.TyForall tyvars  ty'
