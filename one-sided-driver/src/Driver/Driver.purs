module Driver.Driver (
  inferAndRun,
  runStr
) where 


import Driver.Definition (DriverM, DriverState(..), liftErr, debug, addDecl, addVarDecl, addRecDecl)
import Driver.Errors (DriverError(..))
import Common (Modulename, Variable)
import Loc (defaultLoc)
import Environment (Environment(..))

import Syntax.Parsed.Program (Program(..)) as P
import Syntax.Desugared.Program (Program(..),DataDecl(..),VarDecl(..),RecDecl(..)) as D
import Syntax.Desugared.Terms (Command) as D
import Syntax.Kinded.Terms (Command(..)) as K
import Syntax.Kinded.Program (Program(..),emptyProg,isEmpty, DataDecl, VarDecl(..), RecDecl(..)) as K

import Eval.Definition (runEvalM,EvalTrace,emptyTrace)
import Eval.Eval (eval,evalWithTrace)

import Parser.Definition (runSourceParser)
import Parser.Program (parseProgram)

import Dependencies.Definition (runDepM)
import Dependencies.ImportsGraph (depOrderModule)
import Dependencies.VariablesGraph (depOrderProgram)

import Desugar.Definition (runDesugarM)
import Desugar.Program (desugarProgram)

import InferDecl (runDeclM, inferDecl)

import TypeCheck.Definition (runCheckM)
import TypeCheck.Program (checkVarDecl,checkRecDecl)
import TypeCheck.Terms (checkCommand)

import Kinding.Definition (runKindM)
import Kinding.Program (kindVariable,kindRecDecl)
import Kinding.Terms (kindCommand)

import Prelude (bind,pure, ($), (<$>), compare, not, (<>), show)
import Data.List (List(..), elemIndex, sortBy, filter)
import Data.Tuple (Tuple(..),fst,snd)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Map (lookup, fromFoldable, toUnfoldable)
import Data.Traversable (for) 
import Control.Monad.State (gets)
import Control.Monad.Except (throwError)

runStr :: String -> Boolean -> DriverM (Either K.Command EvalTrace) 
runStr progText withTrace = do 
  _ <- debug ("parsing program " <> show progText)
  let progParsed = runSourceParser progText (parseProgram progText) 
  progParsed' <- liftErr progParsed "parsing" 
  prog <- inferProgram progParsed' Nil
  if withTrace then Right <$> runProgramTrace prog else Left <$> runProgram prog

inferAndRun :: P.Program -> List P.Program -> Boolean -> DriverM (Either K.Command EvalTrace)
inferAndRun prog imports withTrace = do
  prog' <- inferProgram prog imports
  if withTrace then Right <$> runProgramTrace prog' else Left <$> runProgram prog'

getInferOrder :: P.Program -> List P.Program -> DriverM (List P.Program)
getInferOrder mn progs = do
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let order = runDepM env (depOrderModule mn progs)
  order' <- liftErr order "dependency order (modules)"
  let indexFun (P.Program p1) (P.Program p2) = compare (elemIndex p1.progName order') (elemIndex p2.progName order')
  pure $ sortBy indexFun progs

inferProgram :: P.Program -> List P.Program -> DriverM K.Program
inferProgram p@(P.Program prog) imports = do
  (Environment env) <- gets (\(MkDriverState s) -> s.drvEnv)
  let mn = prog.progName
  let currProg = fromMaybe (K.emptyProg mn "") (lookup prog.progName env)
  if not $ K.isEmpty currProg  then pure currProg else do 
    let imports' = filter (\(P.Program prog') -> isNothing $ lookup prog'.progName env) imports 
    _ <- debug "ordering imports"
    depsOrdered <- getInferOrder p imports'
    _ <- debug ("infering imports in order: " <> show ((\(P.Program prog') -> prog'.progName) <$> depsOrdered))
    _ <- for depsOrdered (\x -> inferProgram x Nil)
    _ <- debug ("desugaring program " <> show mn) 
    D.Program {progName:mn', progDecls:decls, progVars:vars, progRecs:recs, progMain:main, progSrc:src} <- desugarProg p
    _ <- debug ("inferring declarations in " <> show mn)
    decls' <- for decls (inferDataDecl mn')
    let varNames :: List Variable 
        varNames = fst <$> (toUnfoldable prog.progVars)
    _ <- debug ("ordering variables " <> show varNames <> " in " <> show mn) 
    env' <- gets (\(MkDriverState s) -> s.drvEnv)
    let progOrder = runDepM env' (depOrderProgram p)
    progOrder' <- liftErr progOrder "dependency order (variables)"
    _ <- debug ("inferring variables in order " <> show progOrder')
    let nameFun :: Either D.VarDecl D.RecDecl -> Variable
        nameFun (Left (D.VarDecl var)) = var.varName
        nameFun (Right (D.RecDecl rec)) = rec.recName
    let indexFun v1 v2 = compare (elemIndex (nameFun v1) progOrder') (elemIndex (nameFun v2)  progOrder')
    let varsRecs = (Left <$> (snd <$> toUnfoldable vars)) <> (Right <$> (snd <$> toUnfoldable recs))
    let varsRecsSorted = sortBy indexFun varsRecs
    let inferFun :: Either D.VarDecl D.RecDecl -> DriverM (Either K.VarDecl K.RecDecl)
        inferFun (Left var) = (do
                 var' <-(inferVarDecl mn var)
                 pure $ Left var')
        inferFun (Right rec) = (do
                 rec' <- inferRecDecl mn rec
                 pure $ Right rec')
    varRecsInferred <- for varsRecsSorted inferFun
    let varsInferred = getLefts varRecsInferred
    let recsInferred = getRights varRecsInferred
    let varFun (K.VarDecl decl) = Tuple decl.varName (K.VarDecl decl)
    let recFun (K.RecDecl decl) = Tuple decl.recName (K.RecDecl decl)
    let varMap = fromFoldable (varFun<$>varsInferred)
    let recsMap = fromFoldable (recFun<$>recsInferred)
    _ <- debug "inferring main"
    main' <- for main inferCommand
    pure (K.Program {progName:mn, progDecls:decls',progVars:varMap,progRecs:recsMap,progMain:main',progSrc:src})
    where 
      getLefts :: forall a b. List (Either a b) -> List a
      getLefts Nil = Nil
      getLefts (Cons (Right _) ets) = getLefts ets
      getLefts (Cons (Left a) ets) = Cons a (getLefts ets)

      getRights :: forall a b. List (Either a b) -> List b 
      getRights Nil = Nil
      getRights (Cons (Left _) ets) = getRights ets
      getRights (Cons (Right b) ets) = Cons b (getRights ets)

runProgram :: K.Program -> DriverM K.Command
runProgram (K.Program prog) | isNothing prog.progMain = pure (K.Done defaultLoc) 
runProgram (K.Program prog) = do
  let main = fromMaybe (K.Done defaultLoc) prog.progMain
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  _ <- debug ("evaluating " <> show main) 
  let evaled = runEvalM env (eval main)
  liftErr evaled "evaluation"

runProgramTrace :: K.Program -> DriverM EvalTrace 
runProgramTrace (K.Program prog) | isNothing prog.progMain = pure $ emptyTrace (K.Done defaultLoc)
runProgramTrace (K.Program prog) = do 
  let main = fromMaybe (K.Done defaultLoc) prog.progMain
  env <- gets (\(MkDriverState s) -> s.drvEnv )
  _ <- debug ("evaluating " <> show main)
  let evaled = runEvalM env (evalWithTrace main) 
  liftErr evaled "evaluation (with trace)"

desugarProg :: P.Program -> DriverM D.Program 
desugarProg p@(P.Program prog) = do
  _ <- debug ("desugaring program " <> show prog.progName)
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let prog' = runDesugarM env prog.progName (desugarProgram p)
  liftErr prog' "desugaring"

inferDataDecl :: Modulename -> D.DataDecl -> DriverM K.DataDecl
inferDataDecl mn d@(D.DataDecl decl) = do 
  _ <- debug ("infering declaration " <> show decl.declName) 
  let decl' = runDeclM (inferDecl d)
  decl'' <- liftErr decl' "inferring declaration"
  _ <- addDecl mn decl''
  pure decl''

inferVarDecl :: Modulename -> D.VarDecl -> DriverM K.VarDecl
inferVarDecl _ (D.VarDecl decl) | isNothing decl.varTy = throwError (ErrTypeInference decl.varPos)
inferVarDecl mn v@(D.VarDecl var) = do 
  _<-debug ("type checking variable " <> show var.varName)
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let v' = runCheckM env (checkVarDecl v)
  v'' <- liftErr v'  "type checking"
  let vk = runKindM env (kindVariable v'')
  vk' <- liftErr vk "kind vardecl"
  _ <- addVarDecl mn vk'
  pure vk'

inferRecDecl :: Modulename -> D.RecDecl -> DriverM K.RecDecl 
inferRecDecl _ (D.RecDecl decl) | isNothing decl.recTy = throwError (ErrTypeInference decl.recPos)
inferRecDecl mn r@(D.RecDecl rec) = do
  _ <- debug ("type checking recursive variable " <> show rec.recName)
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let r' = runCheckM env (checkRecDecl r)
  r'' <- liftErr r' "type checking (recursive)"
  let rk = runKindM env (kindRecDecl r'')
  rk' <- liftErr rk "kind recdecl"
  _ <- addRecDecl mn rk'
  pure rk'

inferCommand :: D.Command -> DriverM K.Command
inferCommand c = do 
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let c' = runCheckM env (checkCommand c)
  c'' <- liftErr c' "type checking (command)"
  let ck = runKindM env (kindCommand c'')
  liftErr ck "kinding command"
