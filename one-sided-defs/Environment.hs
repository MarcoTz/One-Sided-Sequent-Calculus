module Environment (
  Environment (..),
  lookupMXtor,
  lookupXtor,
  lookupXtorDecl,
  lookupMVar,
  lookupBody,
  lookupDecl,
  lookupMRec,
  getTypeNames,
  getXtorNames,
  emptyEnv,
  addDeclEnv,
  addVarEnv,
  addRecEnv,
) where 

import Syntax.Typed.Program 
import Syntax.Typed.Terms
import Errors 
import Common
import Loc
import Pretty.Common ()

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map qualified as M
import Data.List (find)

newtype Environment = MkEnv { envDefs :: M.Map Modulename Program }

declErr :: Loc -> Error e => TypeName -> e
declErr loc tyn = toError loc ("Type " <> show tyn <> " not found in environment") 
varErr :: Loc -> Error e => Variable -> e
varErr loc var = toError loc ("Variable " <> show var <> " not found in environment") 
xtorErr :: Loc -> Error e => XtorName -> e
xtorErr loc xtn = toError loc ("Xtor " <> show xtn <> " not found in environment") 

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

type EnvReader e a m = (Error e, MonadError e m, MonadReader Environment m)

getDecls :: Error e => EnvReader e a m => m (M.Map TypeName DataDecl)
getDecls = do
  defs <- asks envDefs
  let progs = snd <$> M.toList defs
  let decls = M.unions (progDecls <$> progs)
  return decls

getVars :: EnvReader e a m => m (M.Map Variable VarDecl)
getVars = do 
  defs <- asks envDefs 
  let progs = snd <$> M.toList defs
  let vars = M.unions (progVars <$> progs) 
  return vars

getRecs :: EnvReader e a m => m (M.Map Variable RecDecl)
getRecs = do 
  defs <- asks envDefs 
  let progs = snd <$> M.toList defs
  let recs = M.unions (progRecs <$> progs)
  return recs

lookupMDecl :: EnvReader e a m => TypeName -> m (Maybe DataDecl)
lookupMDecl tyn = M.lookup tyn <$> getDecls

lookupDecl :: EnvReader e a m => Loc -> TypeName -> m DataDecl 
lookupDecl loc tyn = do   
  mdecl <- lookupMDecl tyn
  case mdecl of 
    Nothing -> throwError (declErr loc tyn)
    Just decl -> return decl

lookupMVar :: EnvReader e a m => Variable -> m (Maybe VarDecl)
lookupMVar v = M.lookup v <$> getVars


lookupMRec :: EnvReader e a m => Variable -> m (Maybe RecDecl)
lookupMRec v = M.lookup v <$> getRecs

lookupBody :: EnvReader e a m => Loc -> Variable -> m Term
lookupBody loc v = do 
  mvar <- lookupMVar v 
  mrec <- lookupMRec v
  case (mvar,mrec) of 
    (Nothing,Nothing) -> throwError (varErr loc v)
    (Just var,_) -> return (varBody var)
    (_,Just rec) -> return (recBody rec)

lookupMXtor :: EnvReader e a m => XtorName -> m (Maybe XtorSig)
lookupMXtor xtn = do
  decls <- getDecls 
  let sigs = concatMap (declXtors . snd) (M.toList decls)
  return $ find (\x -> sigName x == xtn) sigs 

lookupXtor :: EnvReader e a m => Loc -> XtorName -> m XtorSig
lookupXtor loc xtn = do
  mxt <- lookupMXtor xtn
  case mxt of 
    Nothing -> throwError (xtorErr loc xtn) 
    Just xt -> return xt

lookupXtorMDecl :: EnvReader e a m => XtorName -> m (Maybe DataDecl)
lookupXtorMDecl xtn = find (\x -> xtn `elem` (sigName <$> declXtors x)) <$> getDecls

lookupXtorDecl :: EnvReader e a m => Loc -> XtorName -> m DataDecl 
lookupXtorDecl loc xtn = do
  decl <- lookupXtorMDecl xtn
  case decl of 
    Nothing -> throwError (xtorErr loc xtn) 
    Just decl' -> return decl'

getTypeNames :: EnvReader e a m => m [TypeName]
getTypeNames = do 
  decls <- getDecls 
  return $ fst <$> M.toList decls

getXtorNames :: EnvReader e a m => m [XtorName]
getXtorNames = do 
  decls <- getDecls 
  return (sigName <$> concatMap declXtors decls)
