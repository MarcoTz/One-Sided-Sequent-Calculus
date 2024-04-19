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
  getTypes,
  emptyEnv,
  addDeclEnv,
  addVarEnv,
  addRecEnv
) where 

import Common (Modulename, Typename, Variable, Xtorname)
import Loc (Loc)
import Errors (class Error, toError)
import Syntax.Kinded.Program (
  Program(..),DataDecl, getVarTy, VarDecl(..),RecDecl(..), getRecTy, XtorSig(..),
  getXtorname, getXtors, addDeclProgram, getDeclsProgram, addVarProgram, getVarsProgram, addRecProgram, getRecsProgram, emptyProg)
import Syntax.Kinded.Types (Ty)
import Syntax.Kinded.Terms (Term)

import Prelude (class Show, show, (<$>), (<>), bind, pure, ($), (==))
import Data.List (List(..), find, elem, intercalate, concatMap)
import Data.Map (Map,empty,toUnfoldable, lookup, insert, unions)
import Data.Tuple (Tuple(..),fst,snd)
import Data.Maybe (Maybe(..))
import Control.Monad.Reader (class MonadReader, asks)
import Control.Monad.Except (class MonadError, throwError)

newtype Environment = Environment (Map Modulename Program)
instance Show Environment where 
  show (Environment env) = 
    let progs :: List Program 
        progs = snd <$> toUnfoldable env
     in intercalate "\n\n" (show <$> progs)

declErr :: forall e.Loc -> Error e => Typename -> e
declErr loc tyn = toError loc ("Type " <> show tyn <> " not found in environment") 
varErr :: forall e.Loc -> Error e => Variable -> e
varErr loc var = toError loc ("Variable " <> show var <> " not found in environment") 
xtorErr :: forall e. Loc -> Error e => Xtorname -> e
xtorErr loc xtn = toError loc ("Xtor " <> show xtn <> " not found in environment") 

emptyEnv :: Environment
emptyEnv = Environment empty 

getTypes :: Modulename -> Environment -> List (Tuple Variable Ty)
getTypes mn (Environment env) = case lookup mn env of 
  Nothing -> Nil
  Just (Program prog) -> do
   let varDecls :: List VarDecl 
       varDecls = snd <$> toUnfoldable prog.progVars
   let vars :: List (Tuple Variable Ty)
       vars = getVarTy <$> varDecls
   let recDecls :: List RecDecl 
       recDecls = snd <$> toUnfoldable prog.progRecs
   let recs :: List (Tuple Variable Ty) 
       recs = getRecTy <$> recDecls 
   recs<>vars

addDeclEnv :: Modulename -> DataDecl -> Environment -> Environment 
addDeclEnv nm decl (Environment env) = 
  case lookup nm env of 
    Nothing -> let newProg = addDeclProgram decl (emptyProg nm "") in Environment (insert nm newProg env)
    Just prog -> Environment (insert nm (addDeclProgram decl prog) env)

addVarEnv :: Modulename -> VarDecl -> Environment -> Environment 
addVarEnv nm var (Environment env) = 
  case lookup nm env of 
    Nothing -> let newProg = addVarProgram var (emptyProg nm "") in Environment (insert nm newProg env) 
    Just prog -> Environment (insert nm (addVarProgram var prog) env) 

addRecEnv :: Modulename -> RecDecl -> Environment -> Environment 
addRecEnv nm rec (Environment env) = 
  case lookup nm env of 
    Nothing -> let newProg = addRecProgram rec (emptyProg nm "") in Environment (insert nm newProg env)
    Just prog -> Environment (insert nm (addRecProgram rec prog) env)

getDecls :: forall e m. Error e => MonadError e m => MonadReader Environment m => m (Map Typename DataDecl)
getDecls = do
  defs <- asks (\(Environment env) -> env)
  let progs :: List Program 
      progs = snd <$> toUnfoldable defs
  let decls = unions (getDeclsProgram <$> progs)
  pure decls

getVars :: forall e m. Error e => MonadError e m => MonadReader Environment m => m (Map Variable VarDecl)
getVars = do 
  defs <- asks (\(Environment env) -> env) 
  let progs :: List Program
      progs = snd <$> (toUnfoldable defs)
  let vars = unions (getVarsProgram <$> progs) 
  pure vars

getRecs :: forall e m. Error e => MonadError e m => MonadReader Environment m => m (Map Variable RecDecl)
getRecs = do 
  defs <- asks (\(Environment env) -> env) 
  let progs :: List Program
      progs = snd <$> (toUnfoldable defs)
  let recs = unions (getRecsProgram <$> progs)
  pure recs

lookupMDecl :: forall e m. Error e => MonadError e m => MonadReader Environment m => Typename -> m (Maybe DataDecl)
lookupMDecl tyn = lookup tyn <$> getDecls

lookupDecl :: forall e m. Error e => MonadError e m => MonadReader Environment m => Loc -> Typename -> m DataDecl 
lookupDecl loc tyn = do   
  mdecl <- lookupMDecl tyn
  case mdecl of 
    Nothing -> throwError (declErr loc tyn)
    Just decl -> pure decl

lookupMVar :: forall e m. Error e => MonadError e m => MonadReader Environment m => Variable -> m (Maybe VarDecl)
lookupMVar v = lookup v <$> getVars


lookupMRec :: forall e m. Error e => MonadError e m => MonadReader Environment m => Variable -> m (Maybe RecDecl)
lookupMRec v = lookup v <$> getRecs

lookupBody :: forall e m. Error e => MonadError e m => MonadReader Environment m => Loc -> Variable -> m Term
lookupBody loc v = do 
  mvar <- lookupMVar v 
  mrec <- lookupMRec v
  case Tuple mvar mrec of 
    Tuple Nothing Nothing -> throwError (varErr loc v)
    Tuple (Just (VarDecl var)) _ -> pure (var.varBody)
    Tuple _ (Just (RecDecl rec)) -> pure (rec.recBody)

lookupMXtor :: forall e m. Error e => MonadError e m => MonadReader Environment m => Xtorname -> m (Maybe XtorSig)
lookupMXtor xtn = do
  decls <- getDecls 
  let declLs :: List DataDecl 
      declLs = snd <$> (toUnfoldable decls)
  let xtors :: List XtorSig 
      xtors = concatMap getXtors declLs 
  pure $ find (\(XtorSig x) -> x.sigName == xtn) xtors

lookupXtor :: forall e m. Error e => MonadError e m => MonadReader Environment m => Loc -> Xtorname -> m XtorSig
lookupXtor loc xtn = do
  mxt <- lookupMXtor xtn
  case mxt of 
    Nothing -> throwError (xtorErr loc xtn) 
    Just xt -> pure xt

lookupXtorMDecl :: forall e m. Error e => MonadError e m => MonadReader Environment m => Xtorname -> m (Maybe DataDecl)
lookupXtorMDecl xtn = find (\x -> xtn `elem` (getXtorname <$> getXtors x)) <$> getDecls

lookupXtorDecl :: forall e m. Error e => MonadError e m => MonadReader Environment m => Loc -> Xtorname -> m DataDecl 
lookupXtorDecl loc xtn = do
  decl <- lookupXtorMDecl xtn
  case decl of 
    Nothing -> throwError (xtorErr loc xtn) 
    Just decl' -> pure decl'

getTypeNames :: forall e m. Error e => MonadError e m => MonadReader Environment m => m (List Typename)
getTypeNames = do 
  decls <- getDecls 
  pure $ fst <$> toUnfoldable decls

getXtorNames :: forall e m. Error e => MonadError e m => MonadReader Environment m => m (List Xtorname)
getXtorNames = do 
  decls <- getDecls 
  let declList :: List DataDecl 
      declList = snd <$> (toUnfoldable decls)
  let xtors = concatMap getXtors declList
  pure (getXtorname <$> xtors)
