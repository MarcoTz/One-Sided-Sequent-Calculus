module Environment where 

import Syntax.Typed.Program 
import Syntax.Typed.Terms
import Errors 
import Common 

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map qualified as M
import Data.List (find)

newtype Environment = MkEnv { envDefs :: M.Map Modulename Program }

emptyEnv :: Environment
emptyEnv = MkEnv M.empty 

addDeclEnv :: Modulename -> DataDecl -> Environment -> Environment 
addDeclEnv nm decl (MkEnv defs) = 
  case M.lookup nm defs of 
    Nothing -> let newProg = addDeclProgram decl (emptyProg nm) in MkEnv (M.insert nm newProg defs) 
    Just prog -> MkEnv (M.insert nm (addDeclProgram decl prog) defs)

addVarEnv :: Modulename -> VarDecl -> Environment -> Environment 
addVarEnv nm var (MkEnv defs) = 
  case M.lookup nm defs of 
    Nothing -> let newProg = addVarProgram var (emptyProg nm) in MkEnv (M.insert nm newProg defs) 
    Just prog -> MkEnv (M.insert nm (addVarProgram var prog) defs) 

addRecEnv :: Modulename -> RecDecl -> Environment -> Environment 
addRecEnv nm rec (MkEnv defs) = 
  case M.lookup nm defs of 
    Nothing -> let newProg = addRecProgram rec (emptyProg nm) in MkEnv (M.insert nm newProg defs)
    Just prog -> MkEnv (M.insert nm (addRecProgram rec prog) defs)

type EnvReader a m = (MonadError Error m, MonadReader Environment m)

getDecls :: EnvReader a m => m (M.Map TypeName DataDecl)
getDecls = do
  defs <- asks envDefs
  let progs = snd <$> M.toList defs
  let decls = M.unions (progDecls <$> progs)
  return decls

getVars :: EnvReader a m => m (M.Map Variable VarDecl)
getVars = do 
  defs <- asks envDefs 
  let progs = snd <$> M.toList defs
  let vars = M.unions (progVars <$> progs) 
  return vars

getRecs :: EnvReader a m => m (M.Map Variable RecDecl)
getRecs = do 
  defs <- asks envDefs 
  let progs = snd <$> M.toList defs
  let recs = M.unions (progRecs <$> progs)
  return recs

lookupMDecl :: EnvReader a m => TypeName -> m (Maybe DataDecl)
lookupMDecl tyn = M.lookup tyn <$> getDecls

lookupDecl :: EnvReader a m => TypeName -> m DataDecl 
lookupDecl tyn = do   
  mdecl <- lookupMDecl tyn
  case mdecl of 
    Nothing -> throwError (ErrMissingDecl tyn "lookupDecl (Env)")
    Just decl -> return decl

lookupMVar :: EnvReader a m => Variable -> m (Maybe VarDecl)
lookupMVar v = M.lookup v <$> getVars

lookupVar :: EnvReader a m => Variable -> m VarDecl
lookupVar v = do 
  mvar <- lookupMVar v 
  case mvar of 
    Nothing -> throwError (ErrMissingVar v "lookupVar (Env)")
    Just decl -> return decl

lookupMRec :: EnvReader a m => Variable -> m (Maybe RecDecl)
lookupMRec v = M.lookup v <$> getRecs

lookupRec :: EnvReader a m => Variable -> m RecDecl 
lookupRec v = do 
  mrec <- lookupMRec v 
  case mrec of 
    Nothing -> throwError (ErrMissingVar v "lookupRec (Env)")
    Just decl -> return decl

lookupBody :: EnvReader a m => Variable -> m Term
lookupBody v = do 
  mvar <- lookupMVar v 
  mrec <- lookupMRec v
  case (mvar,mrec) of 
    (Nothing,Nothing) -> throwError (ErrMissingVar v "lookupbody (env)")
    (Just var,_) -> return (varBody var)
    (_,Just rec) -> return (recBody rec)

lookupMXtor :: EnvReader a m => XtorName -> m (Maybe XtorSig)
lookupMXtor xtn = do
  decls <- getDecls 
  let sigs = concatMap (declXtors . snd) (M.toList decls)
  return $ find (\x -> sigName x == xtn) sigs 

lookupXtor :: EnvReader a m => XtorName -> m XtorSig
lookupXtor xtn = do
  mxt <- lookupMXtor xtn
  case mxt of 
    Nothing -> throwError (ErrMissingXtor xtn "lookupXtor (Env)") 
    Just xt -> return xt

lookupXtorMDecl :: EnvReader a m => XtorName -> m (Maybe DataDecl)
lookupXtorMDecl xtn = find (\x -> xtn `elem` (sigName <$> declXtors x)) <$> getDecls

lookupXtorDecl :: EnvReader a m => XtorName -> m DataDecl 
lookupXtorDecl xtn = do
  decl <- lookupXtorMDecl xtn
  case decl of 
    Nothing -> throwError (ErrMissingXtor xtn "lookupXtorDecl (Env)") 
    Just decl' -> return decl'

getTypeNames :: EnvReader a m => m [TypeName]
getTypeNames = do 
  decls <- getDecls 
  return $ fst <$> M.toList decls

getXtorNames :: EnvReader a m => m [XtorName]
getXtorNames = do 
  decls <- getDecls 
  return (sigName <$> concatMap declXtors decls)
