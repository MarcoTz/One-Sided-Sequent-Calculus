module Environment (
  Environment (..),
  lookupMXtor,
  lookupXtor,
  lookupXtorDecl,
  lookupMVar,
  lookupBody,
  lookupDecl,
  getVars,
  getTypeNames,
  getXtorNames,
  getTypes,
  emptyEnv,
  addDeclEnv,
  addVarEnv
) where 

import Common (Modulename, Typename, Variable, Xtorname)
import Loc (Loc)
import Errors (class Error, toError)
import Syntax.Kinded.Program (
  Program(..),DataDecl(..), VarDecl(..),XtorSig(..),
  addDeclProgram, addVarProgram, emptyProg)
import Syntax.Kinded.Types (Ty)
import Syntax.Kinded.Terms (Term,getType)

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
getTypes mn (Environment env) = 
  case lookup mn env of 
    Nothing -> Nil
    Just (Program prog) -> do
      let varDecls :: List VarDecl 
          varDecls = snd <$> toUnfoldable prog.progVars
      (\(VarDecl v) -> Tuple v.varName (getType v.varBody)) <$> varDecls

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

getDecls :: forall e m. Error e => MonadError e m => MonadReader Environment m => m (Map Typename DataDecl)
getDecls = do
  defs <- asks (\(Environment env) -> env)
  let progs :: List Program 
      progs = snd <$> toUnfoldable defs
  let decls = unions ((\(Program p) -> p.progDecls) <$> progs)
  pure decls

getVars :: forall e m. Error e => MonadError e m => MonadReader Environment m => m (Map Variable VarDecl)
getVars = do 
  defs <- asks (\(Environment env) -> env) 
  let progs :: List Program
      progs = snd <$> (toUnfoldable defs)
  let vars = unions ((\(Program p) -> p.progVars) <$> progs) 
  pure vars

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

lookupVar :: forall e m.Error e => MonadError e m => MonadReader Environment m => Loc -> Variable -> m VarDecl
lookupVar loc v = do 
  mvar <- lookupMVar v
  case mvar of 
    Nothing -> throwError (varErr loc v)
    Just var -> pure var

lookupBody :: forall e m. Error e => MonadError e m => MonadReader Environment m => Loc -> Variable -> m Term
lookupBody loc v = (\(VarDecl var) -> var.varBody) <$> lookupVar loc v

getXtors :: forall e m. Error e => MonadError e m => MonadReader Environment m => m (List XtorSig)
getXtors = do
  decls <- getDecls 
  let declList :: List DataDecl
      declList = snd <$> toUnfoldable decls
  pure $ concatMap (\(DataDecl d) -> d.declXtors) declList

lookupMXtor :: forall e m. Error e => MonadError e m => MonadReader Environment m => Xtorname -> m (Maybe XtorSig)
lookupMXtor xtn = do
  xtors <- getXtors
  pure $ find (\(XtorSig x) -> x.sigName == xtn) xtors

lookupXtor :: forall e m. Error e => MonadError e m => MonadReader Environment m => Loc -> Xtorname -> m XtorSig
lookupXtor loc xtn = do
  mxt <- lookupMXtor xtn
  case mxt of 
    Nothing -> throwError (xtorErr loc xtn) 
    Just xt -> pure xt

lookupXtorMDecl :: forall e m. Error e => MonadError e m => MonadReader Environment m => Xtorname -> m (Maybe DataDecl)
lookupXtorMDecl xtn = do
  decls <- getDecls
  let declLs :: List DataDecl
      declLs = snd <$> toUnfoldable decls
  let inDecl :: DataDecl -> Boolean
      inDecl (DataDecl decl) = xtn `elem` ((\(XtorSig sig) -> sig.sigName) <$> decl.declXtors)
  pure $ find inDecl declLs

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
  xtors <- getXtors 
  pure $ (\(XtorSig sig) -> sig.sigName) <$> xtors

