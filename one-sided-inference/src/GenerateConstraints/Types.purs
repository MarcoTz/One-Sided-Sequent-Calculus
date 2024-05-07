module GenerateConstraints.Types (
  genConstraintsTy,
  genConstraintsKindedTy
)where 

import GenerateConstraints.Definition (GenM,addTyvar)
import GenerateConstraints.Errors (GenerateError(..))
import Loc (Loc)
import Common (VariantVar(..),Kind(..),varianceEvalOrder, defaultEo)
import Errors (zipWithError)
import Environment (lookupDecl)
import Syntax.Typed.Types (Ty(..),KindedTy(..)) as T 
import Syntax.Desugared.Types (Ty(..),KindedTy(..)) as D 
import Syntax.Kinded.Program (DataDecl(..)) as K

import Prelude (($),(<$>),pure,bind)
import Data.Either (Either(..))
import Data.Traversable (for)
import Control.Monad.Except (throwError)

genConstraintsTy :: Loc -> D.Ty -> GenM T.Ty
genConstraintsTy _ (D.TyVar v) = do
  _ <- addTyvar v 
  pure $ T.TyVar v
genConstraintsTy loc (D.TyDecl tyn args) = do 
  (K.DataDecl decl) <- lookupDecl loc tyn 
  let argPols = (\(VariantVar v) -> MkKind (varianceEvalOrder v.variantVariance (defaultEo decl.declType))) <$> decl.declArgs
  let argsZipped = zipWithError args argPols (ErrTyArity loc tyn)
  case argsZipped of 
    Right err -> throwError err
    Left _ -> do 
      args' <- for args (genConstraintsTy loc)
      pure $ T.TyDecl tyn args' 
genConstraintsTy loc (D.TyCo ty) = do 
  ty' <- genConstraintsTy loc ty 
  pure $ T.TyCo ty'
genConstraintsTy loc (D.TyShift ty) = do
  ty' <- genConstraintsTy loc ty 
  pure $ T.TyShift ty' 
genConstraintsTy loc (D.TyForall args ty) = T.TyForall args <$> genConstraintsTy loc ty

genConstraintsKindedTy :: Loc -> D.KindedTy -> GenM T.KindedTy
genConstraintsKindedTy loc (D.KindedTy kty) = do
    ty' <- genConstraintsTy loc kty.kindedTy
    pure $ T.KindedTy {kindedKind:kty.kindedKind,kindedTy:ty'}
