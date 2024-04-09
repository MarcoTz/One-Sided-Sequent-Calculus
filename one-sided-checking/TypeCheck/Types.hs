module TypeCheck.Types (
  checkKindedTy,
  checkType,
  tryCheckType
) 
where 

import TypeCheck.Definition
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Program   qualified as T
import Errors
import Common
import Loc
import Environment

import Control.Monad
import Control.Monad.Except

checkType :: Loc -> D.Ty -> Kind -> CheckM T.Ty
checkType loc ty knd = do
  mty <- tryCheckType loc ty knd
  case mty of 
    Left ty' -> return ty'
    Right err -> throwError err

tryCheckType :: Loc -> D.Ty -> Kind -> CheckM (Either T.Ty CheckerError)
tryCheckType loc ty MkKindVar{} = return $ Right (ErrKindVar loc ty)
tryCheckType loc (D.TyVar v) knd = do
  tyVars <- getCheckerTyVars 
  if v `elem` tyVars then return (Left (T.TyVar v knd)) else return (Right (ErrFreeTyVar loc v))

tryCheckType loc (D.TyDecl tyn args) (MkKind eo) = do 
   T.MkData _ _ argVars _  _ <- lookupDecl loc tyn
   polPairs <- zipWithError args (MkKind . (`varianceEvalOrder` eo) . variantVariance <$> argVars) (ErrTypeArity loc tyn) 
   args' <- forM polPairs (uncurry (tryCheckType loc))
   let (args'',es) = liftEitherErrorList args'
   if null es then return (Left (T.TyDecl tyn args'' (MkKind eo))) else return (Right (ErrList loc es))

tryCheckType loc (D.TyCo ty) knd = do
  mty <- tryCheckType loc ty (shiftEvalOrder knd) 
  case mty of 
    Left ty' -> return $ Left (T.TyCo ty')
    Right err -> return $ Right err 
tryCheckType loc (D.TyShift ty) knd = do 
  mty <- tryCheckType loc ty (MkKind CBV)
  case mty of 
    Left ty' -> return (Left $ T.TyShift ty' knd)
    Right err -> return $ Right err
tryCheckType loc (D.TyForall args ty) knd = do
  forM_ args addCheckerTyVar 
  mty <- tryCheckType loc ty knd
  case mty of 
    Left ty' -> return $ Left (T.TyForall args ty')
    Right err -> return $ Right err

checkKindedTy :: Loc -> D.KindedTy -> CheckM T.Ty
checkKindedTy loc (D.KindedTy ty knd) = checkType loc ty knd
