module Driver.Driver (
  inferAndRun,
  runStr
) where 

import Common
import Loc
import Environment
import Driver.Definition
import Syntax.Parsed.Program     qualified as P
import Syntax.Desugared.Program  qualified as D
import Syntax.Desugared.Terms    qualified as D
import Syntax.Typed.Program      qualified as T
import Syntax.Typed.Terms        qualified as T

import Parser.Definition (runSourceParser)
import Parser.Program (parseProgram)

import Dependencies.Definition
import Dependencies.ImportsGraph (depOrderModule)
import Dependencies.VariablesGraph (depOrderProgram)

import Desugar.Definition (runDesugarM)
import Desugar.Program (desugarProgram)

import TypeCheck.Definition
import TypeCheck.Program (checkVarDecl,checkRecDecl) 
import TypeCheck.Terms (checkCommand)

import InferDecl (runDeclM, inferDecl)

import Eval.Definition (runEvalM)
import Eval.Eval (eval)

import Pretty.Parsed ()
import Pretty.TypeInference ()
import Pretty.Environment ()

import Control.Monad.State
import Control.Monad.Except
import Control.Monad
import Data.List (elemIndex,sortBy)
import Data.Maybe (isNothing, fromMaybe)
import Data.Either (isLeft,isRight)
import Data.Map qualified as M


runStr :: String -> DriverM (Maybe T.Command) 
runStr progText = do 
  let progParsed = runSourceParser progText (MkModule "") (parseProgram progText) 
  progParsed' <- liftErr progParsed "parsing" 
  prog <- inferProgram progParsed' []
  runProgram prog

inferAndRun :: P.Program -> [P.Program] -> DriverM (Maybe T.Command)
inferAndRun prog imports = do
  prog' <- inferProgram prog imports
  runProgram prog'

getInferOrder :: P.Program -> [P.Program] -> DriverM [P.Program]
getInferOrder mn progs = do
  env <- gets drvEnv
  let order = runDepM env (depOrderModule mn progs)
  order' <- liftErr order "dependency order (modules)"
  let indexFun p1 p2 = compare (elemIndex (P.progName p1) order') (elemIndex (P.progName p2) order')
  return $ sortBy indexFun progs

inferProgram :: P.Program -> [P.Program] -> DriverM T.Program
inferProgram prog imports = do
  env <- gets drvEnv
  let mn = P.progName prog
  let currProg = fromMaybe (T.emptyProg mn) (M.lookup (P.progName prog) (envDefs env))
  if not $ T.isEmpty currProg  then return currProg else do 
    let imports' = filter (\prog' -> isNothing $ M.lookup (P.progName prog') (envDefs env)) imports 
    debug "ordering imports"
    depsOrdered <- getInferOrder prog imports'
    debug ("infering imports in order: " <> show (P.progName <$> depsOrdered))
    oldDebug <- gets drvDebug
    setDebug False
    forM_ depsOrdered (`inferProgram` [])
    setDebug oldDebug
    debug ("desugaring program " <> show mn) 
    D.MkProgram mn' decls vars recs main src <- desugarProg prog
    debug ("inferring declarations in " <> show mn)
    decls' <- forM decls (inferDataDecl mn')
    debug ("ordering variables " <> show (fst <$> (M.toList . P.progVars) prog) <> " in " <> show mn) 
    env' <- gets drvEnv
    let progOrder = runDepM env' (depOrderProgram prog)
    progOrder' <- liftErr progOrder "dependency order (variables)"
    debug ("inferring variables in order " <> show progOrder')
    let nameFun decl = case decl of Left var -> D.varName var; Right rec -> D.recName rec
    let indexFun v1 v2 = compare (elemIndex (nameFun v1) progOrder') (elemIndex (nameFun v2)  progOrder')
    let varsRecs = (Left <$> (snd <$> M.toList vars)) ++ (Right <$> (snd <$> M.toList recs))
    let varsRecsSorted = sortBy indexFun varsRecs
    let inferFun decl = case decl of Left var -> Left <$> inferVarDecl mn var; Right rec -> Right <$> inferRecDecl mn rec
    varRecsInferred <- forM varsRecsSorted inferFun
    let varsInferred = filter isLeft varRecsInferred
    let recsInferred = filter isRight varRecsInferred
    let varFun d = case d of Left v -> (T.varName v, v); Right _ -> error "cannot happen"
    let recFun d = case d of Right r -> (T.recName r,r); Left _ -> error "cannot happen"
    let varMap = M.fromList (varFun<$>varsInferred)
    let recsMap = M.fromList (recFun<$>recsInferred)
    debug "inferring main"
    main' <- forM main inferCommand
    return (T.MkProgram mn decls' varMap recsMap main' src)

runProgram :: T.Program -> DriverM (Maybe T.Command)
runProgram prog | isNothing (T.progMain prog) = return Nothing
runProgram prog = do
  let main = fromMaybe (T.Done defaultLoc) (T.progMain prog)
  env <- gets drvEnv
  let evaled = runEvalM env (eval main)
  Just <$> liftErr evaled "evaluation"

desugarProg :: P.Program -> DriverM D.Program 
desugarProg prog = do
  debug ("desugaring program " <> show (P.progName prog))
  env <- gets drvEnv
  let prog' = runDesugarM env (P.progName prog) (desugarProgram prog)
  liftErr prog' "desugaring"

inferDataDecl :: Modulename -> D.DataDecl -> DriverM T.DataDecl
inferDataDecl mn decl = do 
  debug ("infering declaration " <> show (D.declName decl)) 
  let decl' = runDeclM (inferDecl decl)
  decl'' <- liftErr decl' "inferring declaration"
  addDecl mn decl''
  return decl''

inferVarDecl :: Modulename -> D.VarDecl -> DriverM T.VarDecl
inferVarDecl _ (D.MkVar loc _ Nothing _) = throwError (ErrTypeInference loc)
inferVarDecl mn v@(D.MkVar _ vn (Just _) _) = do 
  debug ("type checking variable " <> show vn)
  env <- gets drvEnv
  let v' = runCheckM env (checkVarDecl v)
  v'' <- liftErr v'  "type checking"
  addVarDecl mn v''
  return v''

inferRecDecl :: Modulename -> D.RecDecl -> DriverM T.RecDecl 
inferRecDecl _ (D.MkRec loc _ Nothing _) = throwError (ErrTypeInference loc)
inferRecDecl mn r@(D.MkRec _ vn (Just _) _) = do
  debug ("type checking recursive variable " <> show vn)
  env <- gets drvEnv 
  let r' = runCheckM env (checkRecDecl r)
  r'' <- liftErr r' "type checking (recursive)"
  addRecDecl mn r''
  return r''

inferCommand :: D.Command -> DriverM T.Command
inferCommand c = do 
  env <- gets drvEnv
  let c' = runCheckM env (checkCommand c)
  liftErr c' "type checking (command)"
