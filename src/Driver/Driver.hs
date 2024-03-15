module Driver.Driver where 

import Common
import Files
import Errors
import Environment
import Driver.Definition
import Syntax.Parsed.Program     qualified as P
import Syntax.Desugared.Program  qualified as D
import Syntax.Desugared.Terms    qualified as D
import Syntax.Typed.Program      qualified as T
import Syntax.Typed.Terms        qualified as T

import Parser.Definition (runFileParser)
import Parser.Program (parseProgram)

import Dependencies.Definition
import Dependencies.ImportsGraph (depOrderModule)
import Dependencies.VariablesGraph (depOrderProgram)

import Desugar.Definition (runDesugarM)
import Desugar.Program (desugarProgram)

import TypeCheck.Definition
import TypeCheck.Program (checkVarDecl) 
import TypeCheck.Terms (checkCommand)

import TypeInference.InferDecl (runDeclM, inferDecl)

import Eval.Definition (runEvalM)
import Eval.Eval (eval)

import Pretty.Terms ()
import Pretty.Program ()
import Pretty.TypeInference ()
import Pretty.Environment ()

import Control.Monad.State
import Control.Monad.Except
import Control.Monad
import Data.List (elemIndex,sortBy)
import Data.Maybe (isNothing, fromMaybe)
import Data.Map qualified as M


runModule :: Modulename ->  DriverM (Maybe T.Command) 
runModule mn = do
  prog <- inferModule mn
  runProgram prog

inferModule :: Modulename -> DriverM T.Program
inferModule mn = do 
  debug ("loading module " <> show mn) 
  prog <- loadProgram mn
  inferProgram prog

loadProgram :: Modulename -> DriverM P.Program
loadProgram mn = do
  debug ("parsing module " <> show mn)
  progText <- loadModule mn
  let progParsed = runFileParser "" parseProgram progText
  prog <- liftErr progParsed
  unless (P.progName prog == mn) $ throwError (ErrModuleNotFound mn " wrong module name in file") 
  return prog

getInferOrder :: Modulename -> [P.Program] -> DriverM [P.Program]
getInferOrder mn progs = do
  let progList = foldr (\(P.MkProgram mn' _ _ _ imps _) ls -> (mn',imps):ls) [] progs 
  env <- gets drvEnv
  let order = runDepM env (depOrderModule mn progList)
  order' <- liftErr order 
  let indexFun p1 p2 = compare (elemIndex (P.progName p1) order') (elemIndex (P.progName p2) order')
  return $ sortBy indexFun progs

inferProgram :: P.Program -> DriverM T.Program
inferProgram prog = do
  env <- gets drvEnv
  let mn = P.progName prog
  let currProg = fromMaybe (T.emptyProg mn) (M.lookup (P.progName prog) (envDefs env))
  debug (show currProg) 
  if not $ T.isEmpty currProg  then return currProg else do 
    let imports = (\(P.MkImport mn') -> mn') <$> P.progImports prog
    let imports' = filter (\mn' -> isNothing $ M.lookup mn' (envDefs env)) imports 
    debug ("loading imports " <> show imports')
    deps <- forM imports' loadProgram
    debug "ordering imports"
    depsOrdered <- getInferOrder mn deps
    debug ("infering imports in order: " <> show (P.progName <$> depsOrdered))
    oldDebug <- gets drvDebug
    setDebug False
    forM_ depsOrdered inferProgram
    setDebug oldDebug
    debug ("desugaring program " <> show mn) 
    D.MkProgram mn' decls vars main <- desugarProg prog
    debug ("inferring declarations in " <> show mn)
    decls' <- forM decls (inferDataDecl mn')
    debug ("ordering variables in " <> show mn) 
    env' <- gets drvEnv
    let varOrder = runDepM env' (depOrderProgram prog)
    varOrder' <- liftErr varOrder
    debug ("inferring variables in order " <> show varOrder')
    let indexFun v1 v2 = compare (elemIndex (D.varName v1) varOrder') (elemIndex (D.varName v2)  varOrder')
    let vars' = sortBy indexFun (snd <$> M.toList vars)
    vars'' <- forM vars' (inferVarDecl mn)
    let varMap = M.fromList ((\v -> (T.varName v,v))<$>vars'')
    main' <- forM main inferCommand
    return (T.MkProgram mn decls' varMap main')

runProgram :: T.Program -> DriverM (Maybe T.Command)
runProgram (T.MkProgram _ _ _ Nothing) = return Nothing
runProgram (T.MkProgram _ _ _ (Just c)) = do
  env <- gets drvEnv
  let evaled = runEvalM env (eval c)
  Just <$> liftErr evaled

desugarProg :: P.Program -> DriverM D.Program 
desugarProg prog = do
  debug ("desugaring program " <> show (P.progName prog))
  env <- gets drvEnv
  let prog' = runDesugarM env (P.progName prog) (desugarProgram prog)
  liftErr prog'

inferDataDecl :: Modulename -> D.DataDecl -> DriverM T.DataDecl
inferDataDecl mn decl = do 
  debug ("infering declaration " <> show (D.declName decl)) 
  let decl' = runDeclM (inferDecl decl)
  decl'' <- liftErr decl'
  addDecl mn decl''
  return decl''

inferVarDecl :: Modulename -> D.VarDecl -> DriverM T.VarDecl
inferVarDecl _ (D.MkVar _ Nothing _) = throwError (ErrMissingType "inferVarDecl, type inference not implemented yet")
inferVarDecl mn v@(D.MkVar vn (Just _) _) = do 
  debug ("type checking variable " <> show vn)
  env <- gets drvEnv
  let v' = runCheckM env (checkVarDecl v)
  v'' <- liftErr v' 
  addVarDecl mn v''
  return v''

inferCommand :: D.Command -> DriverM T.Command
inferCommand c = do 
  env <- gets drvEnv
  let c' = runCheckM env (checkCommand c)
  liftErr c'
