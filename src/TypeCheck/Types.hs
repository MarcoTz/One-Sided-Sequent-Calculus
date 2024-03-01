module TypeCheck.Types where 

import TypeCheck.Definition
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Program   qualified as T
import Common
import Errors
import Utils
import Environment

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as M

checkType :: D.Ty -> Pol -> CheckM T.Ty
checkType (D.TyVar v) pol = do
  tyVars <- gets checkTyVars
  case M.lookup v tyVars of 
    Nothing -> throwError (ErrMissingTyVar v WhereCheck)
    Just pol' -> 
      if pol == pol' then 
        return (T.TyVar v pol) 
      else throwError (ErrKind (MkKind pol) (MkKind pol') ShouldEq WhereCheck)
checkType (D.TyDecl tyn args) pol = do 
   T.MkDataDecl _ args' pol' _ <- lookupDecl tyn
   if pol /= pol' then throwError (ErrKind (MkKind pol) (MkKind pol') ShouldEq WhereCheck) else do
     zipped <- zipWithError args (snd <$> args') (ErrTyArity tyn WhereCheck)
     args'' <- forM zipped (uncurry checkType)
     return (T.TyDecl tyn args'' pol)

