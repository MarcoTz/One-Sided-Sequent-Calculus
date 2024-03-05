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


import Debug.Trace
import Pretty.Common ()

checkType :: D.Ty -> CheckM T.Ty
checkType (D.TyVar v) = do
  tyVars <- gets checkTyVars
  case M.lookup v tyVars of 
    Nothing -> throwError (ErrMissingTyVar v "checkType TyVar")
    Just pol -> return (T.TyVar v pol)

checkType (D.TyDecl tyn args) = do 
   T.MkDataDecl _ args' pol _ <- lookupDecl tyn
   args'' <- forM args checkType 
   argsZipped <- zipWithError (getKind <$> args') (getKind <$> args'') (ErrTyArity tyn "checkType TyDecl")
   trace (show argsZipped) $ return ()
   unless (all (uncurry (==)) argsZipped) $ throwError (ErrKind ShouldEq "checkType TyDecl")
   return $ T.TyDecl tyn args'' pol
