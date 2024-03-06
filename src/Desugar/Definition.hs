module Desugar.Definition where 

import Errors 
import Common 
import Environment
import Embed.Definition
import Embed.EmbedTyped ()
import Syntax.Typed.Program     qualified as T
import Syntax.Desugared.Program qualified as D
import Syntax.Parsed.Program    qualified as P

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map qualified as M
import Data.List (find)

data DesugarState = MkDesugarState { desCurrDecl :: !(Maybe P.DataDecl), desDone :: !D.Program} 

initialDesugarState :: Modulename -> DesugarState 
initialDesugarState nm = MkDesugarState Nothing (D.emptyProg nm)

newtype DesugarM a = DesugarM { getDesugarM :: ReaderT Environment (StateT DesugarState (Except Error)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DesugarState, MonadError Error, MonadReader Environment)

runDesugarM :: Environment -> Modulename -> DesugarM a -> Either Error a
runDesugarM env nm m = case runExcept (runStateT (runReaderT (getDesugarM m) env) (initialDesugarState nm)) of
  Left err -> Left err 
  Right (x,_) ->  Right x 

varToXtor :: Variable -> XtorName
varToXtor (MkVar v) = MkXtorName v

tyvarToTyName :: TypeVar -> TypeName
tyvarToTyName (MkTypeVar v) = MkTypeName v

setCurrDecl :: P.DataDecl -> DesugarM () 
setCurrDecl decl = modify (MkDesugarState (Just decl) . desDone )

getCurrDecl :: Error -> DesugarM P.DataDecl
getCurrDecl err = do 
  curr <- gets desCurrDecl 
  case curr of 
    Nothing -> throwError err 
    Just decl -> return decl

getMDecl :: TypeName -> DesugarM (Maybe D.DataDecl)
getMDecl tyn = do
  mdecl <- lookupMDecl tyn
  doneDecls <- gets (D.progDecls . desDone)
  case (mdecl,M.lookup tyn doneDecls) of
    (Nothing,Nothing) -> return Nothing 
    (_,Just decl) -> return (Just decl)
    (Just decl,_) -> return $ (Just . (embed :: T.DataDecl -> D.DataDecl)) decl

getMXtor :: XtorName -> DesugarM (Maybe D.XtorSig)
getMXtor xtn = do
  msig <- lookupMXtor xtn
  doneDecls <- gets (D.progDecls . desDone)
  let xtors = concatMap D.declXtors doneDecls
  let msig' = find (\x -> D.sigName x == xtn) xtors
  case (msig,msig') of 
    (Nothing,Nothing) -> return Nothing
    (Just sig,_) -> return $ (Just . (embed :: T.XtorSig -> D.XtorSig)) sig
    (_, Just sig) -> return (Just sig)

getDoneVar :: Variable -> DesugarM D.VarDecl 
getDoneVar v = do 
  doneVars <- gets (D.progVars . desDone)
  case M.lookup v doneVars of 
    Nothing -> throwError (ErrMissingVar v "getDoneVar (desugar)")
    Just vdecl -> return vdecl

addDecl :: D.DataDecl -> DesugarM () 
addDecl decl = modify (\s -> MkDesugarState (desCurrDecl s) (D.addDeclProgram decl (desDone s)))

addVar :: D.VarDecl -> DesugarM ()
addVar var = modify (\s -> MkDesugarState (desCurrDecl s) (D.addVarProgram var (desDone s)))
