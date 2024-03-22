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

data InferDeclError = 
  ErrUndefinedTyVar !TypeVar
  | ErrUndefinedType !TypeName
  | ErrIllegalType !D.Ty

instance Error InferDeclError where 
  getMessage (ErrUndefinedTyVar tyv) = "Type Variable " <> show tyv <> " was not defined" 
  getMessage (ErrUndefinedType tyn) = "Type " <> show tyn <> " was not defined"
  getMessage (ErrIllegalType ty) = "Type " <> show ty <> " is not allowed in data declaration"

  getLoc _ = defaultLoc
  toError = error "not implemented"

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
inferDecl (D.MkData tyn args pol xtors) = do 
  setCurrVars args
  setCurrPol pol
  xtors' <- forM xtors inferXtorSig 
  return $ T.MkData tyn args pol xtors'

inferXtorSig :: D.XtorSig -> DeclM T.XtorSig
inferXtorSig (D.MkXtorSig nm args) = do 
  args' <- forM args inferType 
  return $ T.MkXtorSig nm args'

inferType :: D.Ty -> DeclM T.Ty
inferType (D.TyVar v) = do 
  vars <- gets currVars 
  case M.lookup v vars of 
    Nothing -> throwError (ErrUndefinedTyVar v)
    Just pol -> return $ T.TyVar v pol
inferType (D.TyDecl tyn args) = do
  args' <- forM args inferType
  decls <- gets declsDone 
  case M.lookup tyn decls of 
    Nothing -> do 
      pol <- gets currPol 
      case pol of 
        Nothing -> throwError (ErrUndefinedType tyn)
        Just pol' -> return $ T.TyDecl tyn args' pol'
    Just (T.MkData _ _ pol _) -> return $ T.TyDecl tyn args' pol
inferType (D.TyCo ty) = T.TyCo <$> inferType ty
inferType ty@(D.TyShift _) = throwError (ErrIllegalType ty)
inferType ty@(D.TyForall _ _ ) = throwError (ErrIllegalType ty)
