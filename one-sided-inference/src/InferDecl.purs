module InferDecl (
  runDeclM,
  DeclState,
  inferDecl,
  InferDeclError
) where 

import Loc (Loc)
import Common (EvaluationOrder,Typename, Typevar, VariantVar(..), DeclTy, defaultEo, varianceEvalOrder, shiftEvalOrder)
import Errors (class Error)
import Syntax.Kinded.Program (DataDecl(..),XtorSig(..)) as K
import Syntax.Kinded.Types (Ty(..)) as K
import Syntax.Desugared.Types (Ty(..)) as D
import Syntax.Desugared.Program (DataDecl(..),XtorSig(..)) as D

import Prelude ((<>),(<$>),($), show,bind,pure)
import Data.Map (Map,empty,fromFoldable,lookup)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.List (List)
import Data.Unit (Unit,unit)
import Data.Traversable (for)
import Control.Monad.State (StateT, runStateT,modify, gets)
import Control.Monad.Except (Except, runExcept,throwError)

data DeclState = MkDeclState{
  declsDone :: (Map Typename K.DataDecl),
  currVars :: (Map Typevar EvaluationOrder),
  currEo :: (Maybe EvaluationOrder)
}

initialDeclState :: DeclState 
initialDeclState = MkDeclState {declsDone:empty, currVars:empty, currEo:Nothing}

data InferDeclError = 
  ErrUndefinedTyVar  Loc Typevar 
  | ErrUndefinedType Loc Typename
  | ErrIllegalType   Loc D.Ty 
  | ErrOther         Loc String 

instance Error InferDeclError where 
  getMessage (ErrUndefinedTyVar _ tyv) = "Type Variable " <> show tyv <> " was not defined" 
  getMessage (ErrUndefinedType _ tyn) = "Type " <> show tyn <> " was not defined"
  getMessage (ErrIllegalType _ ty) = "Type " <> show ty <> " is not allowed in data declaration"
  getMessage (ErrOther _ str) = str

  getLocation (ErrUndefinedTyVar loc _) = loc 
  getLocation (ErrUndefinedType loc _) = loc
  getLocation (ErrIllegalType loc _) = loc 
  getLocation (ErrOther loc _) = loc

  toError = ErrOther 

type DeclM a = StateT DeclState (Except InferDeclError) a 

runDeclM :: forall a.DeclM a -> Either InferDeclError a 
runDeclM m = case runExcept (runStateT m initialDeclState) of
  Left err -> Left err 
  Right (Tuple x _) ->  Right x

setCurrVars :: List VariantVar -> DeclTy -> DeclM Unit 
setCurrVars vars isco = do
  let eo = defaultEo isco
  let newM = fromFoldable ((\(VariantVar v) -> (Tuple v.variantVar (varianceEvalOrder v.variantVariance eo))) <$> vars)
  _ <- modify (\(MkDeclState s) -> MkDeclState s{currVars=newM})
  pure unit

setCurrPol :: EvaluationOrder -> DeclM Unit 
setCurrPol eo = do
  _ <- modify (\(MkDeclState s) -> MkDeclState (s{currEo=Just eo}))
  pure unit

inferDecl :: D.DataDecl -> DeclM K.DataDecl
inferDecl (D.DataDecl decl) = do 
  _ <- setCurrVars decl.declArgs decl.declType
  _ <- setCurrPol (defaultEo decl.declType) 
  xtors' <- for decl.declXtors inferXtorSig 
  pure $ K.DataDecl {declPos:decl.declPos,declName:decl.declName,declArgs:decl.declArgs,declType:decl.declType,declXtors:xtors'} 

inferXtorSig :: D.XtorSig -> DeclM K.XtorSig
inferXtorSig (D.XtorSig sig) = do 
  args' <- for sig.sigArgs (\(Tuple pc ty) -> Tuple pc <$> inferType sig.sigPos ty)
  pure $ K.XtorSig {sigPos:sig.sigPos, sigName:sig.sigName, sigArgs:args'}

inferType :: Loc -> D.Ty -> DeclM K.Ty
inferType loc (D.TyVar v) = do 
  vars <- gets (\(MkDeclState s) -> s.currVars)
  case lookup v vars of 
    Nothing -> throwError (ErrUndefinedTyVar loc v)
    Just eo -> pure $ K.TyVar v eo
inferType loc (D.TyDecl tyn args) = do

  args' <- for args (inferType loc)
  decls <- gets (\(MkDeclState s) -> s.declsDone)
  case lookup tyn decls of 
    Nothing -> do 
      eo <- gets (\(MkDeclState s) -> s.currEo)
      case eo of 
        Nothing -> throwError (ErrUndefinedType loc tyn)
        Just eo' -> pure $ K.TyDecl tyn args' eo'
    Just (K.DataDecl decl) -> pure $ K.TyDecl tyn args' (defaultEo decl.declType)
inferType loc (D.TyCo ty) = K.TyCo <$> inferType loc ty
inferType loc (D.TyShift ty) = shiftEvalOrder <$> inferType loc ty 
inferType loc ty@(D.TyForall _ _ ) = throwError (ErrIllegalType loc ty)
