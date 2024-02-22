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

import TypeInference.DataDecl
import TypeInference.GenerateConstraints.Terms
import TypeInference.SolveConstraints.Definition
import TypeInference.SolveConstraints.Solver

import Pretty.Terms ()
import Pretty.Program ()
import Pretty.TypeInference ()

import Control.Monad.State
import Control.Monad
import Data.List (intercalate)
import Data.Text.IO qualified as TIO


inferProgram :: FilePath -> DriverM () 
inferProgram path = do 
  progCont <- liftIO $ TIO.readFile path
  debug ("Parsing file " <> path)
  let progParser = runFileParser "" parseProgram progCont
  prog <- liftErr progParser
  debug ("Successfully parsed " <> path)
  debug "desugaring Program"
  let desugar = runDesugarM (desugarProgram prog)
  prog' <- liftErr desugar
  debug "Desugared Program"
  decls <- inferDecls (D.progDecls prog')
  forM_ decls addDecl
  debug "inferring terms"
  forM_ (D.progVars prog') inferVarDecl


inferDecls :: [D.DataDecl] -> DriverM [T.DataDecl]
inferDecls decls = do
  debug ("checking declarations " <> show decls)
  let checked = runDeclM (checkDecls decls)
  decls' <- liftErr checked
  debug "checked declarations"
  return decls'

inferVarDecl :: D.VarDecl -> DriverM T.VarDecl
inferVarDecl (D.MkVarDecl n t) = do 
  t' <- inferTerm t
  let newDecl = T.MkVarDecl n (T.getType t') t'
  addVar newDecl
  return $ T.MkVarDecl n (T.getType t') t' 


inferCommand :: D.Command -> DriverM T.Command
inferCommand c = do 
  prog <- gets drvEnv
  debug (" Inferring " <> show c <> " with environment " <> show prog)
  (c',ctrs) <- liftErr (runGenCmd prog c)
  debug (" Constraints " <> intercalate "\n" (show <$> ctrs))
  (_,varmap,kndmap) <- liftErr (runSolveM ctrs solve)
  debug (" Substitutions " <> show varmap <> "\n" <> show kndmap)
  return c'

inferTerm :: D.Term -> DriverM T.Term
inferTerm t = do 
  prog <- gets drvEnv 
  debug (" Inferring " <> show t)
  (t',ctrs) <- liftErr (runGenT prog t)
  debug (" Constraints " <> intercalate "\n" (show <$> ctrs))
  (_,varmap,kndmap) <- liftErr (runSolveM ctrs solve)
  debug (" Substitutions " <> show varmap <> "\n" <> show kndmap)
  debug (" Final Type : " <> show (T.getType t'))
  return t'

