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

import TypeInference.GenerateConstraints.Definition (runGenM)
import TypeInference.GenerateConstraints.Terms (genConstraintsCmd, genConstraintsTerm)
import TypeInference.SolveConstraints.Definition (runSolveM)
import TypeInference.SolveConstraints.Solver (solve)
import TypeInference.InferDecl (runDeclM, inferDecls)

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
  let desugar = runDesugarM (desugarProgram prog)
  prog' <- liftErr desugar
  let decls = runDeclM (inferDecls (D.progDecls prog'))
  decls' <- liftErr decls
  forM_ decls' addDecl
  forM_ (D.progVars prog') inferVarDecl


inferVarDecl :: D.VarDecl -> DriverM T.VarDecl
inferVarDecl (D.MkVarDecl n t) = do 
  t' <- inferTerm t
  let newDecl = T.MkVarDecl n (T.getType t') t'
  addVarDecl newDecl
  return $ T.MkVarDecl n (T.getType t') t' 


inferCommand :: D.Command -> DriverM T.Command
inferCommand c = do 
  prog <- gets drvEnv
  debug ("Inferring " <> show c)
  (c',ctrs) <- liftErr (runGenM prog (genConstraintsCmd c))
  debug (show ctrs)
  (_,varmap,kndmap) <- liftErr (runSolveM ctrs solve)
  debug ("Substitutions " <> show varmap)
  debug ("\t" <> show kndmap)
  let c'' = T.substKndVars kndmap (T.substVars varmap c')
  return c''

inferTerm :: D.Term -> DriverM T.Term
inferTerm t = do 
  prog <- gets drvEnv 
  debug (" Inferring " <> show t)
  (t',ctrs) <- liftErr (runGenM prog (genConstraintsTerm t))
  debug (show ctrs) 
  (_,varmap,kndmap) <- liftErr (runSolveM ctrs solve)
  debug ("Substitutions " <> show varmap)
  debug ("\t" <> show kndmap)
  let t'' = T.substKndVars kndmap (T.substVars varmap t')
  debug ("Final Type : " <> show (T.generalize $ T.getType t''))
  return t''

