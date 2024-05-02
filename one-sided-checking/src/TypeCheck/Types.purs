module TypeCheck.Types (
  checkKindedTy,
  checkType
) 
where 

import Loc (Loc) 
import Environment (lookupDecl)
import Errors (zipWithErrorM)
import Syntax.Desugared.Types (Ty(..), KindedTy(..)) as D 
import Syntax.Typed.Types (Ty(..),KindedTy(..)) as T
import Syntax.Kinded.Program (DataDecl(..)) as K
import TypeCheck.Definition (CheckM, getCheckerTyVars, addCheckerTyVar)
import TypeCheck.Errors (CheckerError(..))

import Prelude (bind,pure, ($))
import Data.List (elem)
import Data.Traversable (for)
import Control.Monad.Except (throwError)


checkType :: Loc -> D.Ty -> CheckM T.Ty 
checkType loc (D.TyVar v) = do
  tyVars <- getCheckerTyVars 
  if v `elem` tyVars then pure (T.TyVar v) else throwError (ErrFreeTyVar loc v)

checkType loc (D.TyDecl tyn tyArgs) = do 
   K.DataDecl decl <- lookupDecl loc tyn
   _ <- zipWithErrorM tyArgs decl.declArgs (ErrTypeArity loc tyn) 
   args' <- for tyArgs (checkType loc)
   pure (T.TyDecl tyn args')

checkType loc (D.TyCo ty) = do
  ty' <- checkType loc ty 
  pure (T.TyCo ty')

checkType loc (D.TyShift ty) = do 
  ty' <- checkType loc ty 
  pure (T.TyShift ty')

checkType loc (D.TyForall args ty) = do
  _ <- for args addCheckerTyVar 
  ty' <- checkType loc ty 
  pure (T.TyForall args ty')

checkKindedTy :: Loc -> D.KindedTy -> CheckM T.KindedTy
checkKindedTy loc (D.KindedTy kty) = do 
  ty <- checkType loc kty.kindedTy
  pure $ T.KindedTy {kindedTy:ty,kindedKind:kty.kindedKind}

