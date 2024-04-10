module GenerateConstraints.Types where 

import Loc
import Common
import Errors
import Environment
import GenerateConstraints.Definition
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Program   qualified as T 

import Control.Monad
import Control.Monad.Except

genConstraintsTy :: Loc -> D.Ty -> GenM T.Ty
genConstraintsTy _ (D.TyVar v) = return $ T.TyVar v
genConstraintsTy loc (D.TyDecl tyn args) = do 
  decl <- lookupDecl loc tyn 
  let argPols = MkKind . (`varianceEvalOrder` (defaultEo $ T.declType decl)) . variantVariance <$> T.declArgs decl
  let argsZipped = zipWithError args argPols (ErrTyArity loc tyn)
  case argsZipped of 
    Right err -> throwError err
    Left _ -> do 
      args' <- forM args (genConstraintsTy loc)
      return $ T.TyDecl tyn args' 
genConstraintsTy loc (D.TyCo ty) = do 
  ty' <- genConstraintsTy loc ty 
  return $ T.TyCo ty'
genConstraintsTy loc (D.TyShift ty) = do
  ty' <- genConstraintsTy loc ty 
  return $ T.TyShift ty' 
genConstraintsTy loc (D.TyForall args ty) = T.TyForall args <$> genConstraintsTy loc ty

genConstraintsKindedTy :: Loc -> D.KindedTy -> GenM T.Ty
genConstraintsKindedTy loc (D.KindedTy ty _) = genConstraintsTy loc ty
