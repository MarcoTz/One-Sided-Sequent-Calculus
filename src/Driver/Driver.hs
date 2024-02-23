module Driver.Driver where 

import Driver.Definition
import Syntax.Desugared.Terms qualified as D
import Syntax.Desugared.Program qualified as D
import Syntax.Typed.Terms     qualified as T
import Syntax.Typed.Program   qualified as T

import Parser.Definition
import Parser.Program

import Desugar.Definition
import Desugar.Program

import TypeInference.GenerateConstraints.Definition
import TypeInference.GenerateConstraints.Terms
import TypeInference.GenerateConstraints.Program
import TypeInference.SolveConstraints.Definition
import TypeInference.SolveConstraints.Solver

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
  decls <- inferDecls (D.progDecls prog')
  forM_ decls addDecl
  forM_ (D.progVars prog') inferVarDecl


inferDecls :: [D.DataDecl] -> DriverM [T.DataDecl]
inferDecls decls = do
  prog <- gets drvEnv
  let checked = runGenM prog. genConstraintsDecl <$> decls
  decls' <- forM checked liftErr 
  return (fst <$> decls')

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
  (_,varmap) <- liftErr (runSolveM ctrs solve)
  debug ("Substitutions " <> show varmap)
  let c'' = T.substVars varmap c'
  return c''

inferTerm :: D.Term -> DriverM T.Term
inferTerm t = do 
  prog <- gets drvEnv 
  debug (" Inferring " <> show t)
  (t',ctrs) <- liftErr (runGenM prog (genConstraintsTerm t))
  debug (show ctrs) 
  (_,varmap) <- liftErr (runSolveM ctrs solve)
  debug ("Substitutions " <> show varmap)
  let t'' = T.substVars varmap t'
  debug ("Final Type : " <> show (T.getType t''))
  return t''

