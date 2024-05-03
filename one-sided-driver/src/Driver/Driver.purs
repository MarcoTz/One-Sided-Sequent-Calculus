module Driver.Driver (
  runStr,
  parseProg,
  inferAndRun
) where 


import Driver.Definition (DriverM, DriverState(..), liftErr, debug, addDecl, addVarDecl, inEnv, getProg, withDebug)
import Driver.Errors (DriverError(..))
import Common (Modulename, Variable)
import Loc (defaultLoc)
import Environment (Environment(..))
import StandardLib (libMap)

import Syntax.Parsed.Program (Program(..),Import(..))                  as P
import Syntax.Desugared.Program (Program(..),DataDecl(..),VarDecl(..)) as D
import Syntax.Desugared.Terms (Command)                                as D
import Syntax.Typed.Types (isSubsumed)                                 as T
import Syntax.Typed.Terms (getType)                                    as T
import Syntax.Typed.Program (VarDecl(..))                              as T
import Syntax.Kinded.Terms (Command(..))                               as K
import Syntax.Kinded.Program (Program(..),DataDecl, VarDecl(..))       as K

import Syntax.Typed.Substitution (substTyvars)

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

import Constraints (showConstrs)
import GenerateConstraints.Definition (runGenM) 
import GenerateConstraints.Program (genConstraintsVarDecl)
import GenerateConstraints.Terms (genConstraintsCmd)
import SolveConstraints.Definition (runSolveM,showSubst)
import SolveConstraints.Solver (solve)

import TypeCheck.Definition (runCheckM)
import TypeCheck.Types (checkType)
import TypeCheck.Program (checkVarDecl)

import Kinding.Definition (runKindM)
import Kinding.Program (kindVariable)
import Kinding.Terms (kindCommand)

import Prelude (bind,pure, ($), (<$>), compare, (<>), show, (*>))
import Data.List (List(..), elemIndex, sortBy, filter, intercalate,null)
import Data.Tuple (Tuple(..),fst,snd)
import Data.Either (Either(..))
import Data.String (take,indexOf,Pattern(..),trim)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Map (lookup, fromFoldable, toUnfoldable,isEmpty)
import Data.Unit (Unit,unit)
import Data.Traversable (for) 
import Control.Bind (ifM)
import Control.Monad(unless)
import Control.Monad.State (gets)
import Control.Monad.Except (throwError)

runStr :: Modulename -> String -> Boolean -> DriverM (Either K.Command EvalTrace) 
runStr mn progText withTrace = do 
  progParsed' <- parseProg mn progText
  prog <- inferProgram progParsed' 
  if withTrace then Right <$> runProgramTrace prog else Left <$> runProgram prog

parseProg :: Modulename -> String -> DriverM P.Program 
parseProg mn src = do 
  _ <- debug "------ Parsing ------"
  let srcStripped = trim src
  let progTextShort = take (fromMaybe 10 (indexOf (Pattern "\n") srcStripped)) srcStripped
  _ <- debug ("parsing program from string \"" <> progTextShort <> "...\"") 
  let progParsed = runSourceParser src (parseProgram src)
  prog <- liftErr progParsed mn "parsing"
  _ <- debug ("parsed program \n" <> show prog <> "\n") 
  pure prog

getImports :: P.Program -> DriverM (List P.Program)
getImports (P.Program prog) = do
  _ <- debug ("loading standard library imports")
  let imps = (\(P.Import imp) -> imp.importName) <$> prog.progImports
  let maybeSrcs = (\mn ->Tuple mn (lookup mn libMap)) <$> imps
  let (Tuple notFoundImps foundImps) = splitImps maybeSrcs
  _ <- unless (null notFoundImps) $ throwError (ErrNotStdLib notFoundImps)
  let impNames = fst <$> foundImps
  _ <- debug ("Imports to load: " <> intercalate ", " (show <$> impNames))
  impsParsed <- for foundImps (\(Tuple nm src) -> withDebug false (parseProg nm) src)
  pure impsParsed
  where 
    splitImps :: List (Tuple Modulename (Maybe String)) -> Tuple (List Modulename) (List (Tuple Modulename String))
    splitImps Nil = Tuple Nil Nil 
    splitImps (Cons (Tuple nm Nothing) rst) = let (Tuple notFound found) = splitImps rst in Tuple (Cons nm notFound) found
    splitImps (Cons (Tuple nm (Just src)) rst) = let (Tuple notFound found) = splitImps rst in Tuple notFound (Cons (Tuple nm src) found)

inferProgram :: P.Program -> DriverM K.Program
inferProgram p@(P.Program prog) = ifM (inEnv prog.progName) (getProg prog.progName) (do
  _ <- debug "------ Loading Imports ------"
  imports  <- getImports p
  impsOrdered <- getInferOrder p imports
  _ <- inferImportsOrdered impsOrdered
  _ <- debug ""
  _ <- debug "------ Desugaring Program ------"
  p'@(D.Program prog') <- desugarProg p
  _ <- debug ("Succcessfully desugared program\n" <> show p')
  _ <- debug ""
  _ <- debug "------ Type inference ------"
  _ <- debug ("checking type declarations in " <> show prog'.progName) 
  decls' <- for prog'.progDecls (inferDataDecl prog'.progName)
  _ <- debug ""
  progOrder <- getVarOrder p'
  let indexFun (D.VarDecl var1) (D.VarDecl var2) = compare (elemIndex var1.varName progOrder) (elemIndex var2.varName progOrder)
  let varsSorted = sortBy indexFun (snd <$> toUnfoldable prog'.progVars)
  _ <- debug "inferring types of variables\n"
  typedVars <- for varsSorted (inferVarDecl prog'.progName)
  _ <- debug "------ Kind inference ------"
  kindedVars <- for typedVars (kindVarDecl prog'.progName)
  let varmap = fromFoldable ((\d@(K.VarDecl var) -> Tuple var.varName d) <$> kindedVars)
  main' <- for prog'.progMain (inferCommand prog.progName)
  pure $ K.Program {
       progName:prog.progName, 
       progDecls:decls',
       progVars:varmap,
       progMain:main',
       progSrc:prog.progSrc})

inferAndRun :: P.Program -> DriverM K.Command
inferAndRun p = do 
  p' <- inferProgram p 
  runProgram p' 

getInferOrder :: P.Program -> List P.Program -> DriverM (List P.Program)
getInferOrder (P.Program prog) Nil = 
  debug ("No imports for " <> show prog.progName <> ", skipping import order") *> pure Nil
getInferOrder p@(P.Program prog) progs = do
  _ <- debug ("Ordering Imports for " <> show prog.progName)
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let order = runDepM env (depOrderModule p progs)
  order' <- liftErr order prog.progName "dependency order (modules)"
  let indexFun (P.Program p1) (P.Program p2) = compare (elemIndex p1.progName order') (elemIndex p2.progName order')
  let impsSorted = sortBy indexFun progs
  _ <- debug ("ordered imports: " <> intercalate ", " ((\(P.Program p') -> show p'.progName) <$> impsSorted))
  pure impsSorted

inferImportsOrdered :: List P.Program -> DriverM Unit
inferImportsOrdered Nil =
  debug "No imports to infer" *> pure unit
inferImportsOrdered imports = do 
  (Environment env) <- gets (\(MkDriverState s) -> s.drvEnv)
  let imports' = filter (\(P.Program prog') -> isNothing $ lookup prog'.progName env) imports 
  let impNames = (\(P.Program prog) -> prog.progName) <$> imports'
  _ <- debug ("Loading imports not found in environment: " <> intercalate ", " (show <$> impNames))
  _ <- for imports' (\x -> withDebug false inferProgram x)
  _ <- debug ("Successfully loaded imports")
  pure unit

getVarOrder :: D.Program -> DriverM (List Variable)
getVarOrder (D.Program prog) | isEmpty prog.progVars = pure Nil
getVarOrder p@(D.Program prog) = do
  _ <- debug ("ordering variables in " <> show prog.progName)
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let progOrder = runDepM env (depOrderProgram p)
  progOrder' <- liftErr progOrder prog.progName "dependency order (variables)"
  let orderStr = intercalate ", " (show <$> progOrder')
  _ <- debug ("ordered variables: " <> orderStr)
  pure progOrder' 

desugarProg :: P.Program -> DriverM D.Program 
desugarProg p@(P.Program prog) = do
  _ <- debug ("desugaring program " <> show prog.progName)
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let prog' = runDesugarM env prog.progName (desugarProgram p)
  liftErr prog' prog.progName "desugaring"

inferDataDecl :: Modulename -> D.DataDecl -> DriverM K.DataDecl
inferDataDecl mn d@(D.DataDecl decl) = do 
  _ <- debug ("infering declaration " <> show decl.declName) 
  let decl' = runDeclM (inferDecl d)
  decl'' <- liftErr decl' mn "inferring declaration"
  _ <- addDecl mn decl''
  pure decl''

inferVarDecl :: Modulename -> D.VarDecl -> DriverM T.VarDecl
inferVarDecl mn v@(D.VarDecl var@{varPos:_,varName:_, varBody:_, varTy:Just ty}) = do 
  _<-debug ("type checking variable " <> show var.varName)
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let v' = runCheckM env (checkVarDecl v)
  case v' of 
      Left _ -> do
        _ <- debug ("type checking for variable " <> show var.varName <> " failed, inferring type instead")
        let annotTy = runCheckM env (checkType var.varPos ty) 
        annotTy' <- liftErr annotTy mn "checking type annotation" 
        kv@(T.VarDecl var') <- inferVarDecl mn (D.VarDecl var{varTy=Nothing})
        let inferTy =  T.getType var'.varBody
        if T.isSubsumed annotTy' inferTy then do
          _ <- debug ("Annotated type " <> show annotTy' <> " is correctly subsumed by inferred type " <> show inferTy <> "\n")
          pure kv 
        else throwError (ErrAnnotMismatch var.varPos var.varName annotTy' inferTy)
      Right v''@(T.VarDecl var') -> do
        _ <- debug ("Successfully checked type "  <> show (T.getType var'.varBody) <> " for " <> show var.varName <> "\n")
        pure v''

inferVarDecl mn v@(D.VarDecl var) = do 
  _ <- debug ("inferring type for " <> show var.varName)
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let constr = runGenM env (genConstraintsVarDecl v) 
  (Tuple v' (Tuple tyvars constrs)) <- liftErr constr mn "generate constraints"
  _ <- debug ("generated typevars\n\t" <> intercalate ", " (show <$> tyvars))
  _ <- debug ("generated constraints\n" <> showConstrs constrs)
  let slv = runSolveM constrs solve
  (Tuple _ varmap) <- liftErr slv mn "solve constraints"
  _ <- debug ("solved constraints and got substitution\n " <> showSubst varmap)
  let v''@(T.VarDecl var') = substTyvars varmap v'
  _ <- debug ("Final type for variable " <> show var.varName <> ": " <> show (T.getType var'.varBody) <> "\n")
  pure v''

kindVarDecl :: Modulename -> T.VarDecl -> DriverM K.VarDecl 
kindVarDecl mn v@(T.VarDecl var) = do
  _ <- debug ("inferring kind of variable " <> show var.varName)
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let var' = runKindM env (kindVariable v)
  var'' <- liftErr var' mn "kind vardecl"
  _ <- addVarDecl mn var''
  pure var''

inferCommand :: Modulename -> D.Command -> DriverM K.Command
inferCommand mn c = do 
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  let ctr = runGenM env (genConstraintsCmd c)
  Tuple c' (Tuple _ constrs) <- liftErr ctr mn "generate constraints command"
  let vm = runSolveM constrs solve 
  Tuple _ varmap <- liftErr vm mn "solving constraints command"
  let c'' = substTyvars varmap c'
  let ck = runKindM env (kindCommand c'')
  liftErr ck mn "kinding command (after infer)"

runProgram :: K.Program -> DriverM K.Command
runProgram (K.Program prog) | isNothing prog.progMain = pure (K.Done defaultLoc) 
runProgram (K.Program prog) = do
  let main = fromMaybe (K.Done defaultLoc) prog.progMain
  env <- gets (\(MkDriverState s) -> s.drvEnv)
  _ <- debug ("evaluating main " <> show main) 
  let evaled = runEvalM env (eval main)
  liftErr evaled prog.progName "evaluation"

runProgramTrace :: K.Program -> DriverM EvalTrace 
runProgramTrace (K.Program prog) | isNothing prog.progMain = pure $ emptyTrace (K.Done defaultLoc)
runProgramTrace (K.Program prog) = do 
  let main = fromMaybe (K.Done defaultLoc) prog.progMain
  env <- gets (\(MkDriverState s) -> s.drvEnv )
  _ <- debug ("evaluating " <> show main)
  let evaled = runEvalM env (evalWithTrace main (emptyTrace main)) 
  liftErr evaled prog.progName "evaluation (with trace)"

