module Desugar.Definition (
  DesugarM,
  DesugarState,
  runDesugarM,
  tyvarToTyName,
  varToXtor,
  getDesDefNames,
  getDesMXtor,
  getDesDoneVar,
  getDesDoneProg,
  setDesMain,
  setDesCurrDecl,
  freshVar,
  addDesVar,
  addDesDecl
) where 

import Desugar.Errors (DesugarError(..))
import Common (Variable(..),Modulename,Xtorname(..), Typevar(..), Typename(..))
import Loc (Loc)
import Environment (Environment,lookupMXtor)
import FreeVars.FreeVariables (freeVars,class FreeVariables)
import Syntax.Kinded.Program    (embedXtorSig) as K
import Syntax.Typed.Program (embedXtorSig) as T
import Syntax.Desugared.Program (Program(..),emptyProg,XtorSig(..),VarDecl,DataDecl(..),addDeclProgram, addVarProgram, setMainProgram) as D
import Syntax.Desugared.Terms  (Command) as D
import Syntax.Parsed.Program (DataDecl(..)) as P

import Prelude (bind, (<$>),pure,(==),($),show,(+),(<>))
import Data.Map (toUnfoldable,lookup) 
import Data.Set (toUnfoldable) as S
import Data.List (List(..), find, concatMap,elem)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..),fst,snd)
import Data.Either (Either(..))
import Data.Unit (Unit,unit)
import Control.Monad.State (StateT, runStateT, gets, modify)
import Control.Monad.Except (Except, runExcept,throwError)
import Control.Monad.Reader (ReaderT, runReaderT)

data DesugarState = MkDesugarState { desCurrDecl :: (Maybe P.DataDecl), desDone :: D.Program, desVars :: List Variable} 

initialDesugarState :: Modulename -> DesugarState 
initialDesugarState nm = MkDesugarState {desCurrDecl:Nothing, desDone:(D.emptyProg nm ""), desVars:Nil}

type DesugarM a = ReaderT Environment (StateT DesugarState (Except DesugarError)) a


runDesugarM :: forall a.Environment -> Modulename -> DesugarM a -> Either DesugarError a
runDesugarM env nm m = case runExcept (runStateT (runReaderT m env) (initialDesugarState nm)) of
  Left err -> Left err 
  Right (Tuple x _) ->  Right x 

varToXtor :: Variable -> Xtorname
varToXtor (Variable v) = Xtorname v 

tyvarToTyName :: Typevar -> Typename
tyvarToTyName (Typevar v) = Typename v 

freshVar :: forall a. FreeVariables a => a -> DesugarM Variable 
freshVar t = do 
  desVars <- gets (\(MkDesugarState s) -> s.desVars)
  let frVars = S.toUnfoldable $ freeVars t
  let allVars = desVars <> frVars
  let newV = newVar 0 allVars 
  _ <- modify (\(MkDesugarState s) -> MkDesugarState s{desVars = Cons newV s.desVars})
  pure newV
  where 
    newVar :: Int -> List Variable -> Variable 
    newVar i vars = 
      let freshV = Variable (show i) in 
      if freshV `elem` vars then newVar (i+1) vars 
      else freshV

getDesDoneProg :: DesugarM D.Program
getDesDoneProg = gets (\(MkDesugarState s) -> s.desDone)

getDesDefNames :: DesugarM (List Typename)
getDesDefNames = do
  doneProg <-  getDesDoneProg 
  let doneNames = (\(D.Program prog) -> fst <$> toUnfoldable prog.progDecls)  doneProg
  curr <- gets (\(MkDesugarState s) -> s.desCurrDecl)
  case curr of 
    Nothing -> pure doneNames 
    Just (P.DataDecl decl) -> pure (Cons decl.declName doneNames)

setDesCurrDecl :: P.DataDecl -> DesugarM Unit 
setDesCurrDecl decl = do 
  _ <- modify (\(MkDesugarState s) -> MkDesugarState s{desCurrDecl=Just decl}) 
  pure unit

getDesMXtor :: Xtorname -> DesugarM (Maybe D.XtorSig)
getDesMXtor xtn = do
  msig <- lookupMXtor xtn
  doneProg <- gets (\(MkDesugarState s) -> s.desDone)
  let doneDecls = (\(D.Program prog) -> snd <$> (toUnfoldable  prog.progDecls)) doneProg
  let xtors = concatMap (\(D.DataDecl d) -> d.declXtors) doneDecls
  let msig' = find (\(D.XtorSig x) -> x.sigName == xtn) xtors
  case (Tuple msig msig') of 
    (Tuple Nothing Nothing) -> pure Nothing
    (Tuple (Just sig) _) -> pure $ Just (T.embedXtorSig (K.embedXtorSig sig))
    (Tuple _ (Just sig)) -> pure $ Just sig 

getDesDoneVar :: Loc -> Variable -> DesugarM D.VarDecl
getDesDoneVar loc v = do 
  doneProg <- gets (\(MkDesugarState s) -> s.desDone) 
  let doneVars = (\(D.Program prog) -> prog.progVars) doneProg
  case lookup v doneVars of 
      Nothing -> throwError (ErrVariable loc v)
      Just vdecl -> pure vdecl

addDesDecl :: D.DataDecl -> DesugarM Unit 
addDesDecl decl = do 
  _ <- modify (\(MkDesugarState s) -> MkDesugarState s{desDone=D.addDeclProgram decl s.desDone})
  pure unit

addDesVar :: D.VarDecl -> DesugarM Unit
addDesVar var = do 
  _ <- modify (\(MkDesugarState s) -> MkDesugarState s{desDone=D.addVarProgram var s.desDone})
  pure unit

setDesMain :: D.Command -> DesugarM Unit 
setDesMain m = do 
  _ <- modify (\(MkDesugarState s) -> MkDesugarState s{desDone=D.setMainProgram m s.desDone}) 
  pure unit 
