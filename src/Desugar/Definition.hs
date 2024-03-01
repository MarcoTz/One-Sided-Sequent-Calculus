module Desugar.Definition where 

import Errors 
import Common 
import Environment
import Embed.Definition
import Embed.EmbedTyped ()
import Syntax.Typed.Program qualified as T
import Syntax.Desugared.Program qualified as D
import Syntax.Parsed.Program qualified as P

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map qualified as M


data DesugarState = MkDesugarState { desCurrDecl :: !(Maybe P.DataDecl), desDone :: !D.Program} 

initialDesugarState :: DesugarState 
initialDesugarState = MkDesugarState Nothing D.emptyProg

newtype DesugarM a = DesugarM { getDesugarM :: ReaderT Environment (StateT DesugarState (Except Error)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DesugarState, MonadError Error, MonadReader Environment)

runDesugarM :: Environment -> DesugarM a -> Either Error a
runDesugarM env m = case runExcept (runStateT (runReaderT (getDesugarM m) env) initialDesugarState) of
  Left err -> Left err 
  Right (x,_) ->  Right x 

setCurrDecl :: P.DataDecl -> DesugarM () 
setCurrDecl decl = modify (MkDesugarState (Just decl) . desDone )

getCurrDecl :: Error -> DesugarM P.DataDecl
getCurrDecl err = do 
  curr <- gets desCurrDecl 
  case curr of 
    Nothing -> throwError err 
    Just decl -> return decl

getTynPol :: TypeName -> DesugarM Pol 
getTynPol tyn = do 
  decl <- lookupDecl tyn 
  return $ T.declPol decl

getDecl :: TypeName -> DesugarM D.DataDecl
getDecl tyn = do
  mdecl <- lookupMDecl tyn
  doneDecls <- gets (D.progDecls . desDone)
  case (mdecl,M.lookup tyn doneDecls) of
    (Nothing,Nothing) -> throwError (ErrDeclUndefined tyn)
    (_,Just decl) -> return decl
    (Just decl,_) -> return $ (embed :: T.DataDecl -> D.DataDecl) decl

getDoneVar :: Variable -> DesugarM D.VarDecl 
getDoneVar v = do 
  doneVars <- gets (D.progVars . desDone)
  case M.lookup v doneVars of 
    Nothing -> throwError (ErrVarUndefined v)
    Just vdecl -> return vdecl

addDecl :: D.DataDecl -> DesugarM () 
addDecl decl = modify (\s -> MkDesugarState (desCurrDecl s) (D.addDeclProgram decl (desDone s)))

addVar :: D.VarDecl -> DesugarM ()
addVar var = modify (\s -> MkDesugarState (desCurrDecl s) (D.addVarProgram var (desDone s)))
