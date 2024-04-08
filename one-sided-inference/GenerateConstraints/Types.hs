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

genConstraintsTy :: Loc -> D.Ty -> Kind -> GenM T.Ty
genConstraintsTy _ (D.TyVar v) knd = return $ T.TyVar v knd
genConstraintsTy loc (D.TyDecl tyn args) knd = do 
  decl <- lookupDecl loc tyn 
  let argPols = MkKind . (`varianceEvalOrder` (defaultEo $ T.declType decl)) . variantVariance <$> T.declArgs decl
  argsZipped <- zipWithError args argPols (ErrTyArity loc tyn)
  args' <- forM argsZipped (uncurry (genConstraintsTy loc))
  return $ T.TyDecl tyn args' knd 
genConstraintsTy loc (D.TyCo ty) knd = do 
  ty' <- genConstraintsTy loc ty (shiftEvalOrder knd)
  return $ T.TyCo ty'
genConstraintsTy loc (D.TyShift ty) knd = do
  ty' <- genConstraintsTy loc ty (shiftEvalOrder knd)
  return $ T.TyShift ty' knd
genConstraintsTy loc (D.TyForall args ty) knd = T.TyForall args <$> genConstraintsTy loc ty knd

genConstraintsPolTy :: Loc -> D.KindedTy -> GenM T.Ty
genConstraintsPolTy loc (D.KindedTy ty knd) = genConstraintsTy loc ty knd
