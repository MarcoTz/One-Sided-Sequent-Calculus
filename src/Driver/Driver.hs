module Driver.Driver where 

import Driver.Definition
import Syntax.Parsed.Program     qualified as P
import Syntax.Parsed.Terms       qualified as P
import Syntax.Desugared.Terms    qualified as D
import Syntax.Typed.Terms        qualified as T
import Syntax.Typed.Program      qualified as T
import Syntax.Typed.Types        qualified as T
import Syntax.Typed.Substitution qualified as T

import Parser.Definition (runFileParser)
import Parser.Program (parseProgram)

import Desugar.Definition (runDesugarM)
import Desugar.Program (desugarDecl)
import Desugar.Terms (desugarTerm)

import TypeInference.GenerateConstraints.Definition (runGenM)
import TypeInference.GenerateConstraints.Terms (genConstraintsCmd, genConstraintsTerm)
import TypeInference.SolveConstraints.Definition (runSolveM)
import TypeInference.SolveConstraints.Solver (solve)
import TypeInference.InferDecl (runDeclM, inferDecl)

import Environment 

import Pretty.Terms ()
import Pretty.Program ()
import Pretty.TypeInference ()

import Control.Monad.State
import Control.Monad
import Data.Text.IO qualified as TIO
import Data.Map qualified as M


inferProgram :: FilePath -> DriverM () 
inferProgram path = do 
  progCont <- liftIO $ TIO.readFile path
  debug ("Inferring program in file " <> path)
  let progParser = runFileParser "" parseProgram progCont
  prog <- liftErr progParser
  env <- gets drvEnv
  forM_ (P.progDecls prog) (\d -> do 
    let desugared = runDesugarM env (desugarDecl d)
    desugared' <- liftErr desugared
    let inferred = runDeclM (inferDecl desugared')
    inferred' <- liftErr inferred
    addDecl inferred')
  forM_ (P.progVars prog) inferVarDecl

inferVarDecl :: P.VarDecl -> DriverM T.VarDecl
inferVarDecl (P.MkVarDecl n t) = do 
  t' <- inferTerm t
  let newDecl = T.MkVarDecl n (T.getType t') t'
  addVarDecl newDecl
  return $ T.MkVarDecl n (T.getType t') t' 


inferCommand :: D.Command -> DriverM T.Command
inferCommand c = do 
  env <- gets drvEnv
  debug ("Inferring " <> show c)
  let prog = T.MkProgram (snd <$> (M.toList . envDecls) env) (snd <$> (M.toList . envVars) env)
  (c',ctrs) <- liftErr (runGenM prog (genConstraintsCmd c))
  debug (show ctrs)
  (_,varmap,kndmap) <- liftErr (runSolveM ctrs solve)
  debug ("Substitutions " <> show varmap)
  debug ("\t" <> show kndmap)
  let c'' = T.substVars varmap c'
  return c''

inferTerm :: P.Term -> DriverM T.Term
inferTerm t = do 
  env <- gets drvEnv 
  let t' = runDesugarM env (desugarTerm t)
  t'' <- liftErr t'
  debug (" Inferring " <> show t)
  let prog = T.MkProgram (snd <$> (M.toList . envDecls) env) (snd <$> (M.toList . envVars) env)
  (t''',ctrs) <- liftErr (runGenM prog (genConstraintsTerm t''))
  debug (show ctrs) 
  (_,varmap,kndmap) <- liftErr (runSolveM ctrs solve)
  debug ("Substitutions " <> show varmap)
  debug ("\t" <> show kndmap)
  let t'''' = T.substVars varmap t'''
  debug ("Final Type : " <> show (T.generalize $ T.getType t''''))
  return t''''

