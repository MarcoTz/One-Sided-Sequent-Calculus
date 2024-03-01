module Driver.Driver where 
import Driver.Definition
import Syntax.Desugared.Terms    qualified as D
import Syntax.Desugared.Program  qualified as D
import Syntax.Typed.Terms        qualified as T
import Syntax.Typed.Program      qualified as T
import Syntax.Typed.Types        qualified as T
import Syntax.Typed.Substitution qualified as T

import Parser.Definition (runFileParser)
import Parser.Program (parseProgram)

import Desugar.Definition (runDesugarM)
import Desugar.Program (desugarProgram)

import TypeCheck.Definition
import TypeCheck.Terms (checkTerm) 

import TypeInference.GenerateConstraints.Definition (runGenM)
import TypeInference.GenerateConstraints.Terms (genConstraintsCmd, genConstraintsTerm)
import TypeInference.SolveConstraints.Definition (runSolveM)
import TypeInference.SolveConstraints.Solver (solve)
import TypeInference.InferDecl (runDeclM, inferDecl)


import Pretty.Terms ()
import Pretty.Program ()
import Pretty.TypeInference ()

import Control.Monad.State
import Control.Monad
import Data.Text.IO qualified as TIO


inferProgram :: FilePath -> DriverM () 
inferProgram path = do 
  progCont <- liftIO $ TIO.readFile path
  debug ("Inferring program in file " <> path)
  let progParser = runFileParser "" parseProgram progCont
  prog <- liftErr progParser
  debug "parsed program successfully"
  env <- gets drvEnv
  let prog' = runDesugarM env (desugarProgram prog)
  prog'' <- liftErr prog'
  debug "desugared Program successfully"  
  forM_ (D.progDecls prog'') (\d -> do 
    let inferred = runDeclM (inferDecl d)
    inferred' <- liftErr inferred
    addDecl inferred')
  debug "inferred declarations sucessfully"
  forM_ (D.progVars prog'') inferVarDecl


inferVarDecl :: D.VarDecl -> DriverM T.VarDecl
inferVarDecl (D.MkVar n Nothing t) = do 
  t' <- inferTerm t
  let newDecl = T.MkVarDecl n (T.generalize $ T.getType t') t'
  addVarDecl newDecl
  return newDecl 
inferVarDecl (D.MkVar n (Just ty) t) = do 
  env <- gets drvEnv
  let t' =  runCheckM env (checkTerm t ty)
  t'' <- liftErr t'
  return (T.MkVarDecl n (T.generalize (T.getType t'')) t'')



inferCommand :: D.Command -> DriverM T.Command
inferCommand c = do 
  env <- gets drvEnv
  debug ("Inferring " <> show c)
  (c',ctrs) <- liftErr (runGenM env (genConstraintsCmd c))
  debug (show ctrs)
  (_,varmap,kndmap) <- liftErr (runSolveM ctrs solve)
  debug ("Substitutions " <> show varmap)
  debug ("\t" <> show kndmap)
  let c'' = T.substVars varmap c'
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
  let t'' = T.substVars varmap t'
  debug ("Final Type : " <> show (T.generalize $ T.getType t''))
  return t''

