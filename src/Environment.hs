module Environment where 

import Syntax.Typed.Program 
import Errors 
import Common 

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map qualified as M

data Environment = MkEnv { envDecls :: !(M.Map TypeName DataDecl), envVars :: !(M.Map Variable VarDecl) }

emptyEnv :: Environment
emptyEnv = MkEnv M.empty M.empty

insertDecl :: Environment -> DataDecl -> Environment 
insertDecl (MkEnv decls vars) decl = MkEnv (M.insert (declNm decl) decl decls) vars

insertVar :: Environment -> VarDecl -> Environment
insertVar (MkEnv decls vars) var = MkEnv decls (M.insert (varNm var) var vars)

type EnvReader a m = (MonadError Error m, MonadReader Environment m)

lookupDecl :: EnvReader a m => TypeName -> m DataDecl
lookupDecl tyn = do 
  decls <- asks envDecls 
  case M.lookup tyn decls of 
    Nothing -> throwError (ErrDeclUndefined tyn)
    Just decl -> return decl

lookupVar :: EnvReader a m => Variable -> m VarDecl
lookupVar v = do 
  vars <- asks envVars 
  case M.lookup v vars of 
    Nothing -> throwError (ErrVarUndefined v)
    Just decl -> return decl

tyExists :: EnvReader a m => TypeName -> m Bool 
tyExists tyn = do 
  decls <- asks envDecls
  case M.lookup tyn decls of 
    Nothing -> return False 
    Just _ -> return True

xtorExists :: EnvReader a m => XtorName -> m Bool
xtorExists xtn = do 
  decls <- asks envDecls 
  let sigs = sigName <$> concatMap declSig decls
  return $ xtn `elem` sigs
