module Driver.Driver where 

import Driver.Definition
import Syntax.Parsed.Terms qualified as S
import Syntax.Parsed.Program qualified as S
import Syntax.Typed.Terms qualified as T
import Syntax.Typed.Program qualified as T
import Parser.Definition
import Parser.Program
import TypeInference.DataDecl
import TypeInference.GenerateConstraints
import TypeInference.SolveConstraints
import Pretty.Terms ()
import Pretty.Program ()
import Pretty.TypeInference ()

import Control.Monad.State
import Control.Monad
import Data.List (intercalate)
import Data.Text.IO qualified as T



inferProgram :: FilePath -> DriverM () 
inferProgram path = do 
  progCont <- liftIO $ T.readFile path
  debug ("Parsing file " <> path)
  let progParser = runFileParser "" parseProgram progCont
  prog <- liftErr progParser
  debug ("Successfully parsed " <> path)
  debug ("parsed declarations " <> show (S.progDecls prog))
  debug ("parsed terms " <> show (S.progVars prog))
  decls <- inferDecls (S.progDecls prog)
  forM_ decls addDecl
  debug "inferring terms"
  forM_ (S.progVars prog) inferVarDecl


inferDecls :: [S.DataDecl] -> DriverM [T.DataDecl]
inferDecls decls = do
  debug ("checking declarations " <> show decls)
  let checked = runDeclM (checkDecls decls)
  decls' <- liftErr checked
  debug ("checked declarations " <> show decls')
  return decls'

inferVarDecl :: S.VarDecl -> DriverM T.VarDecl
inferVarDecl (S.MkVarDecl n t) = do 
  t' <- inferTerm t
  let newDecl = T.MkVarDecl n (T.getType t') t'
  addVar newDecl
  return $ T.MkVarDecl n (T.getType t') t' 


inferCommand :: S.Command -> DriverM T.Command
inferCommand c = do 
  prog <- gets drvEnv
  debug (" Inferring " <> show c <> " with environment " <> show prog)
  (c',ctrs) <- liftErr (runGenCmd prog c)
  debug (" Constraints " <> intercalate "\n" (show <$> ctrs))
  (_,varmap,kndmap) <- liftErr (runSolve ctrs)
  debug (" Substitutions " <> show varmap <> "\n" <> show kndmap)
  return c'

inferTerm :: S.Term -> DriverM T.Term
inferTerm t = do 
  prog <- gets drvEnv 
  debug (" Inferring " <> show t <> " with environment " <> show prog)
  (t',ctrs) <- liftErr (runGenT prog t)
  debug (" Constraints " <> intercalate "\n" (show <$> ctrs))
  (_,varmap,kndmap) <- liftErr (runSolve ctrs)
  debug (" Substitutions " <> show varmap <> "\n" <> show kndmap)
  debug (" Final Type : " <> show (T.getType t'))
  return t'

