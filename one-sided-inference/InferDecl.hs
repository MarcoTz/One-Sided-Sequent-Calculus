module InferDecl (
  runDeclM,
  inferDecl
) where 

import Syntax.Desugared.Program qualified as D
import Syntax.Desugared.Types   qualified as D
import Syntax.Kinded.Program     qualified as K
import Syntax.Kinded.Types       qualified as K
import Embed.EmbedDesugared ()
import Errors
import Common
import Loc
import Pretty.Common ()
import Pretty.Desugared ()

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as M

data DeclState = MkDeclState{
  declsDone :: !(M.Map Typename K.DataDecl),
  currVars :: !(M.Map Typevar EvaluationOrder),
  currEo :: !(Maybe EvaluationOrder)
}

initialDeclState :: DeclState 
initialDeclState = MkDeclState M.empty M.empty Nothing

data InferDeclError where 
  ErrUndefinedTyVar :: Loc -> Typevar -> InferDeclError
  ErrUndefinedType :: Loc -> Typename -> InferDeclError
  ErrIllegalType :: Loc -> D.Ty -> InferDeclError 
  ErrOther :: Loc -> String -> InferDeclError

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

newtype DeclM a = DeclM { getGenM :: StateT DeclState (Except InferDeclError) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DeclState, MonadError InferDeclError)

runDeclM :: DeclM a -> Either InferDeclError a 
runDeclM m = case runExcept (runStateT (getGenM m) initialDeclState) of
  Left err -> Left err 
  Right (x, _) ->  Right x

setCurrVars :: [VariantVar] -> DeclTy -> DeclM () 
setCurrVars vars isco = do
  let eo = defaultEo isco
  let newM = M.fromList ((\(VariantVar tyv var) -> (tyv,varianceEvalOrder var eo)) <$> vars)
  modify (\s -> MkDeclState (declsDone s) newM (currEo s))

setCurrPol :: EvaluationOrder -> DeclM () 
setCurrPol eo = modify (\s -> MkDeclState (declsDone s) (currVars s) (Just eo))

inferDecl :: D.DataDecl -> DeclM K.DataDecl
inferDecl (D.MkData loc tyn args isco xtors) = do 
  setCurrVars args isco
  setCurrPol (defaultEo isco) 
  xtors' <- forM xtors inferXtorSig 
  return $ K.MkData loc tyn args isco xtors'

inferXtorSig :: D.XtorSig -> DeclM K.XtorSig
inferXtorSig (D.MkXtorSig loc nm args) = do 
  args' <- forM args (inferType loc)
  return $ K.MkXtorSig loc nm args'

inferType :: Loc -> D.Ty -> DeclM K.Ty
inferType loc (D.TyVar v) = do 
  vars <- gets currVars 
  case M.lookup v vars of 
    Nothing -> throwError (ErrUndefinedTyVar loc v)
    Just eo -> return $ K.TyVar v (MkKind eo)
inferType loc (D.TyDecl tyn args) = do

  args' <- forM args (inferType loc)
  decls <- gets declsDone 
  case M.lookup tyn decls of 
    Nothing -> do 
      eo <- gets currEo 
      case eo of 
        Nothing -> throwError (ErrUndefinedType loc tyn)
        Just eo' -> return $ K.TyDecl tyn args' (MkKind eo')
    Just (K.MkData _ _ _ isco _) -> return $ K.TyDecl tyn args' (MkKind . defaultEo $ isco)
inferType loc (D.TyCo ty) = K.TyCo <$> inferType loc ty
inferType loc (D.TyShift ty) = shiftEvalOrder <$> inferType loc ty 
inferType loc ty@(D.TyForall _ _ ) = throwError (ErrIllegalType loc ty)
