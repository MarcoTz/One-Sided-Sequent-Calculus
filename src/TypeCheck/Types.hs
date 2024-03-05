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


checkType :: D.Ty -> CheckM T.Ty
checkType (D.TyVar v)= do
  tyVars <- gets checkTyVars
  case M.lookup v tyVars of 
    Nothing -> throwError (ErrMissingTyVar v "checkType TyVar")
    Just pol -> return (T.TyVar v pol)

checkType (D.TyDecl tyn args) = do 
   T.MkDataDecl _ argVars pol' _ <- lookupDecl tyn
   args' <- forM args checkType
   polPairs <- zipWithError (getKind <$> args') (getKind <$> argVars) (ErrTyArity tyn " checkType TyDecl")
   unless (all (uncurry (==)) polPairs) $ throwError (ErrKind ShouldEq " checkType TyDecl")
   return $ T.TyDecl tyn args' pol' 

checkType (D.TyCo ty) = T.TyCo <$> checkType ty 
