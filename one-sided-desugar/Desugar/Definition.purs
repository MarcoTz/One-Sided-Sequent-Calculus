module Desugar.Definition (
  DesugarM,
  runDesugarM,
  tyvarToTyName,
  varToXtor,
  getDesDefNames,
  getDesMXtor,
  getDesDoneVar,
  getDesDoneProg,
  setDesMain,
  setDesCurrDecl,
  addDesRec,
  addDesVar,
  addDesDecl,
  DesugarError (..),
) where 

import Desugar.Errors
import Common 
import Loc
import Environment
import Embed.Definition
import Embed.EmbedKinded()
import Syntax.Kinded.Program     qualified as T
import Syntax.Desugared.Program qualified as D
import Syntax.Desugared.Terms   qualified as D
import Syntax.Parsed.Program    qualified as P
import Pretty.Desugared ()

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map qualified as M
import Data.List (find)

data DesugarState = MkDesugarState { desCurrDecl :: !(Maybe P.DataDecl), desDone :: !D.Program} 

initialDesugarState :: Modulename -> DesugarState 
initialDesugarState nm = MkDesugarState Nothing (D.emptyProg nm)

newtype DesugarM a = DesugarM { getDesugarM :: ReaderT Environment (StateT DesugarState (Except DesugarError)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DesugarState, MonadError DesugarError, MonadReader Environment)


runDesugarM :: Environment -> Modulename -> DesugarM a -> Either DesugarError a
runDesugarM env nm m = case runExcept (runStateT (runReaderT (getDesugarM m) env) (initialDesugarState nm)) of
  Left err -> Left err 
  Right (x,_) ->  Right x 

varToXtor :: Variable -> Xtorname
varToXtor (Variable v) = Xtorname v

tyvarToTyName :: Typevar -> Typename
tyvarToTyName (Typevar v) = Typename v

getDesDoneProg :: DesugarM D.Program
getDesDoneProg = gets desDone

getDesDefNames :: DesugarM [Typename]
getDesDefNames = do
  doneNames <-  gets (map fst .  M.toList . D.progDecls . desDone)
  curr <- gets desCurrDecl
  case curr of 
    Nothing -> return doneNames 
    Just (P.MkData _ tyn _ _ _) -> return (tyn : doneNames)

setDesCurrDecl :: P.DataDecl -> DesugarM () 
setDesCurrDecl decl = modify (MkDesugarState (Just decl) . desDone )

getDesMXtor :: Xtorname -> DesugarM (Maybe D.XtorSig)
getDesMXtor xtn = do
  msig <- lookupMXtor xtn
  doneDecls <- gets (D.progDecls . desDone)
  let xtors = concatMap D.declXtors doneDecls
  let msig' = find (\x -> D.sigName x == xtn) xtors
  case (msig,msig') of 
    (Nothing,Nothing) -> return Nothing
    (Just sig,_) -> return $ (Just . (embed :: T.XtorSig -> D.XtorSig)) sig
    (_, Just sig) -> return (Just sig)

getDesDoneVar :: Loc -> Variable -> DesugarM (Either D.VarDecl D.RecDecl)
getDesDoneVar loc v = do 
  doneVars <- gets (D.progVars . desDone)
  doneRecs <- gets (D.progRecs . desDone)
  case (M.lookup v doneVars,M.lookup v doneRecs) of 
    (Nothing,Nothing) -> throwError (ErrVariable loc v)
    (Just vdecl,_) -> return $ Left vdecl
    (_,Just rdecl) -> return $ Right rdecl

addDesDecl :: D.DataDecl -> DesugarM () 
addDesDecl decl = modify (\s -> MkDesugarState (desCurrDecl s) (D.addDeclProgram decl (desDone s)))

addDesVar :: D.VarDecl -> DesugarM ()
addDesVar var = modify (\s -> MkDesugarState (desCurrDecl s) (D.addVarProgram var (desDone s)))

addDesRec :: D.RecDecl -> DesugarM () 
addDesRec rec = modify (\s -> MkDesugarState (desCurrDecl s) (D.addRecProgram rec (desDone s)))

setDesMain :: D.Command -> DesugarM () 
setDesMain m = modify (\s -> MkDesugarState (desCurrDecl s) (D.setMainProgram m (desDone s)))
