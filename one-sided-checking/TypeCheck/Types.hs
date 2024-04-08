module TypeCheck.Types (
  checkKindedTy
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
checkType loc ty MkKindVar{} = throwError (ErrKindVar loc ty)
checkType loc (D.TyVar v) knd = do
  tyVars <- getCheckerTyVars 
  if v `elem` tyVars then return $ T.TyVar v knd else throwError (ErrFreeTyVar loc v)

checkType loc (D.TyDecl tyn args) (MkKind eo) = do 
   T.MkData _ _ argVars _  _ <- lookupDecl loc tyn
   polPairs <- zipWithError args (MkKind . (`varianceEvalOrder` eo) . variantVariance <$> argVars) (ErrTypeArity loc tyn) 
   args' <- forM polPairs (uncurry (checkType loc))
   return $ T.TyDecl tyn args' (MkKind eo)

checkType loc (D.TyCo ty) knd = T.TyCo <$> checkType loc ty (shiftEvalOrder knd)
checkType loc (D.TyShift ty) knd = (`T.TyShift` knd) <$> checkType loc ty (MkKind CBV) 
checkType loc (D.TyForall args ty) knd = do
  forM_ args addCheckerTyVar 
  T.TyForall args <$> checkType loc ty knd

checkKindedTy :: Loc -> D.KindedTy -> CheckM T.Ty
checkKindedTy loc (D.KindedTy ty knd) = checkType loc ty knd
