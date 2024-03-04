module TypeInference.InferDecl where 

import Syntax.Desugared.Program qualified as D
import Syntax.Desugared.Types   qualified as D
import Syntax.Typed.Program     qualified as T
import Syntax.Typed.Types       qualified as T
import Errors
import Common

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


newtype DeclM a = DeclM { getGenM :: StateT DeclState (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DeclState, MonadError Error)

runDeclM :: DeclM a -> Either Error a 
runDeclM m = case runExcept (runStateT (getGenM m) initialDeclState) of
  Left err -> Left err 
  Right (x, _) ->  Right x

addDecl :: T.DataDecl -> DeclM ()
addDecl decl = do
  currDecls <- gets declsDone 
  let newM = M.insert (T.declName decl) decl currDecls
  modify (\s -> MkDeclState newM (currVars s) (currPol s))

setCurrVars :: [PolVar] -> DeclM () 
setCurrVars vars = do
  let newM = M.fromList ((\(MkPolVar tyv pol) -> (tyv,pol)) <$> vars)
  modify (\s -> MkDeclState (declsDone s) newM (currPol s))

setCurrPol :: Pol -> DeclM () 
setCurrPol pol = modify (\s -> MkDeclState (declsDone s) (currVars s) (Just pol))

inferDecls :: [D.DataDecl] -> DeclM [T.DataDecl] 
inferDecls decls = 
  forM decls (\d -> do
    d' <- inferDecl d 
    addDecl d'
    return d')

inferDecl :: D.DataDecl -> DeclM T.DataDecl
inferDecl (D.MkData tyn args pol xtors) = do 
  setCurrVars args
  setCurrPol pol
  xtors' <- forM xtors inferXtorSig 
  return $ T.MkDataDecl tyn args pol xtors'

inferXtorSig :: D.XtorSig -> DeclM T.XtorSig
inferXtorSig (D.MkXtorSig nm args) = do 
  args' <- forM args inferType 
  return $ T.MkXtorSig nm args'

inferType :: D.Ty -> DeclM T.Ty
inferType (D.TyVar v) = do 
  vars <- gets currVars 
  case M.lookup v vars of 
    Nothing -> throwError (ErrMissingTyVar v WhereDecl)
    Just pol -> return $ T.TyVar v pol
inferType (D.TyDecl tyn args) = do
  args' <- forM args inferType
  decls <- gets declsDone 
  case M.lookup tyn decls of 
    Nothing -> do 
      pol <- gets currPol 
      case pol of 
        Nothing -> throwError (ErrMissingDecl tyn WhereDecl)
        Just pol' -> return $ T.TyDecl tyn args' pol'
    Just (T.MkDataDecl _ _ pol _) -> return $ T.TyDecl tyn args' pol
inferType (D.TyForall _ _) = error "not implemented (inferType)"
