module InferDecl (
  runDeclM,
  inferDecl
) where 

import Syntax.Desugared.Program qualified as D
import Syntax.Desugared.Types   qualified as D
import Syntax.Typed.Program     qualified as T
import Syntax.Typed.Types       qualified as T
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
  declsDone :: !(M.Map TypeName T.DataDecl),
  currVars :: !(M.Map TypeVar Pol),
  currPol :: !(Maybe Pol)
}

initialDeclState :: DeclState 
initialDeclState = MkDeclState M.empty M.empty Nothing

data InferDeclError where 
  ErrUndefinedTyVar :: Loc -> TypeVar -> InferDeclError
  ErrUndefinedType :: Loc -> TypeName -> InferDeclError
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

setCurrVars :: [PolVar] -> DeclM () 
setCurrVars vars = do
  let newM = M.fromList ((\(MkPolVar tyv pol) -> (tyv,pol)) <$> vars)
  modify (\s -> MkDeclState (declsDone s) newM (currPol s))

setCurrPol :: Pol -> DeclM () 
setCurrPol pol = modify (\s -> MkDeclState (declsDone s) (currVars s) (Just pol))

inferDecl :: D.DataDecl -> DeclM T.DataDecl
inferDecl (D.MkData loc tyn args pol xtors) = do 
  setCurrVars args
  setCurrPol pol
  xtors' <- forM xtors inferXtorSig 
  return $ T.MkData loc tyn args pol xtors'

inferXtorSig :: D.XtorSig -> DeclM T.XtorSig
inferXtorSig (D.MkXtorSig loc nm args) = do 
  args' <- forM args (inferType loc)
  return $ T.MkXtorSig loc nm args'

inferType :: Loc -> D.Ty -> DeclM T.Ty
inferType loc (D.TyVar v) = do 
  vars <- gets currVars 
  case M.lookup v vars of 
    Nothing -> throwError (ErrUndefinedTyVar loc v)
    Just pol -> return $ T.TyVar v pol
inferType loc (D.TyDecl tyn args) = do
  args' <- forM args (inferType loc)
  decls <- gets declsDone 
  case M.lookup tyn decls of 
    Nothing -> do 
      pol <- gets currPol 
      case pol of 
        Nothing -> throwError (ErrUndefinedType loc tyn)
        Just pol' -> return $ T.TyDecl tyn args' pol'
    Just (T.MkData _ _ _ pol _) -> return $ T.TyDecl tyn args' pol
inferType loc (D.TyCo ty) = T.TyCo <$> inferType loc ty
inferType loc ty@(D.TyShift _) = throwError (ErrIllegalType loc ty)
inferType loc ty@(D.TyForall _ _ ) = throwError (ErrIllegalType loc ty)
