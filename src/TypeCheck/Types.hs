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


checkTypeScheme :: D.TypeScheme -> CheckM T.TypeScheme
checkTypeScheme (D.MkTypeScheme tyvars ty) = do
  ty' <- checkType ty 
  return $ T.MkTypeScheme tyvars  ty'

checkType :: D.Ty -> CheckM T.Ty
checkType (D.TyVar v) = do
  tyVars <- gets checkTyVars
  case M.lookup v tyVars of 
    Nothing -> throwError (ErrMissingTyVar v WhereCheck)
    Just pol -> return (T.TyVar v pol)  
checkType (D.TyDecl tyn args) = do 
   T.MkDataDecl _ args' pol _ <- lookupDecl tyn
   args'' <- forM args checkType
   zipped <- zipWithError ((\(MkPolVar _ pol') -> pol') <$> args') (getKind <$> args'') (ErrTyArity tyn WhereCheck)
   forM_ zipped (\(pol1,pol2) -> if pol1 == pol2 then return () else throwError (ErrKind (MkKind pol1) (MkKind pol2) ShouldEq WhereCheck)) 
   return $ T.TyDecl tyn args'' pol
