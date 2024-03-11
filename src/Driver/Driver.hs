module Driver.Driver where 

import Common
import Files
import Errors
import Driver.Definition
import Syntax.Parsed.Program     qualified as P
import Syntax.Desugared.Terms    qualified as D
import Syntax.Desugared.Program  qualified as D
import Syntax.Typed.Terms        qualified as T
import Syntax.Typed.Program      qualified as T
import Syntax.Typed.Substitution qualified as T

import Parser.Definition (runFileParser)
import Parser.Program (parseProgram)

import Desugar.Definition (runDesugarM)
import Desugar.Program (desugarProgram)

import TypeCheck.Definition
import TypeCheck.Program (checkVarDecl) 

import TypeInference.GenerateConstraints.Definition (runGenM)
import TypeInference.GenerateConstraints.Terms (genConstraintsCmd, genConstraintsTerm)
import TypeInference.SolveConstraints.Definition (runSolveM)
import TypeInference.SolveConstraints.Solver (solve)
import TypeInference.InferDecl (runDeclM, inferDecl)

import Pretty.Terms ()
import Pretty.Program ()
import Pretty.TypeInference ()
import Pretty.Environment ()

import Control.Monad.State
import Control.Monad.Except
import Control.Monad


inferProgram :: Modulename -> DriverM () 
inferProgram mn = do 
  loaded <- gets drvLoaded
  when (mn `elem` loaded) $ throwError (ErrDuplModule mn "Driver inferProgram")
  progCont <- loadModule mn
  debug ("Inferring module " <> show mn)
  let progParser = runFileParser "" parseProgram progCont
  prog <- liftErr progParser
  addLoaded mn
  debug ("successfully parsed program: \n" <> show prog <> "\n") 
  forM_ (P.importName <$> P.progImports prog) inferProgram
  env <- gets drvEnv
  let prog' = runDesugarM env mn (desugarProgram prog)
  prog'' <- liftErr prog'
  debug ("desugared Program successfully: \n" <> show prog'')
  forM_ (D.progDecls prog'') (\d -> do 
    let inferred = runDeclM (inferDecl d)
    inferred' <- liftErr inferred
    addDecl mn inferred')
  debug "inferred declarations sucessfully"
  forM_ (D.progVars prog'') (inferVarDecl mn)

inferVarDecl :: Modulename -> D.VarDecl -> DriverM T.VarDecl
inferVarDecl mn (D.MkVar n _ Nothing t) = do 
  t' <- inferTerm t
  -- this is not yet correct
  let newDecl = T.MkVar n [] (T.getType t') t'
  addVarDecl mn newDecl
  return newDecl 
inferVarDecl mn v@(D.MkVar _ _ (Just _) _) = do 
  env <- gets drvEnv
  let v' = runCheckM env (checkVarDecl v)
  v'' <- liftErr v' 
  addVarDecl mn v''
  return v''

inferCommand :: D.Command -> DriverM T.Command
inferCommand c = do 
  env <- gets drvEnv
  debug ("Inferring " <> show c)
  (c',ctrs) <- liftErr (runGenM env (genConstraintsCmd c))
  debug (show ctrs)
  (_,varmap,kndmap) <- liftErr (runSolveM ctrs solve)
  debug ("Substitutions " <> show varmap)
  debug ("\t" <> show kndmap)
  let c'' = T.substTyVars varmap c'
  return c''

inferTerm :: D.Term -> DriverM T.Term
inferTerm t = do 
  env <- gets drvEnv 
  debug (" Inferring " <> show t)
  (t',ctrs) <- liftErr (runGenM env (genConstraintsTerm t))
  debug (show ctrs) 
  (_,varmap,kndmap) <- liftErr (runSolveM ctrs solve)
  debug ("Substitutions " <> show varmap)
  debug ("\t" <> show kndmap)
  let t'' = T.substTyVars varmap t'
  debug ("Final Type : " <> show (T.getType t''))
  return t''

