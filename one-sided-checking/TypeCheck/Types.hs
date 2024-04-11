module TypeCheck.Types (
  checkKindedTy,
  checkType,
) 
where 

import TypeCheck.Definition
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Syntax.Kinded.Program  qualified as K
import Errors
import Loc
import Environment

import Control.Monad
import Control.Monad.Except


checkType :: Loc -> D.Ty -> CheckM T.Ty 
checkType loc (D.TyVar v) = do
  tyVars <- getCheckerTyVars 
  if v `elem` tyVars then return (T.TyVar v) else throwError (ErrFreeTyVar loc v)

checkType loc (D.TyDecl tyn tyArgs) = do 
   K.MkData _ _ argVars _  _ <- lookupDecl loc tyn
   _ <- zipWithErrorM tyArgs argVars (ErrTypeArity loc tyn) 
   args' <- forM tyArgs (checkType loc)
   return (T.TyDecl tyn args')

checkType loc (D.TyCo ty) = do
  ty' <- checkType loc ty 
  return (T.TyCo ty')

checkType loc (D.TyShift ty) = do 
  ty' <- checkType loc ty 
  return (T.TyShift ty')

checkType loc (D.TyForall args ty) = do
  forM_ args addCheckerTyVar 
  ty' <- checkType loc ty 
  return (T.TyForall args ty')

checkKindedTy :: Loc -> D.KindedTy -> CheckM T.KindedTy
checkKindedTy loc (D.KindedTy ty knd) = (`T.KindedTy` knd) <$> checkType loc ty
