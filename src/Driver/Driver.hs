module Driver.Driver where 

import Driver.Definition
import Untyped.Syntax qualified as S
import Untyped.Program qualified as S
import Typed.Syntax qualified as T
import Parser.Definition
import Parser.Program
import TypeInference.GenerateConstraints
import TypeInference.SolveConstraints
import Pretty () 

import Control.Monad.State
import Control.Monad.Except
import Data.List (intercalate)
import Data.Text.IO qualified as T


inferProgram :: FilePath -> DriverM S.Program
inferProgram path = do 
  progCont <- liftIO $ T.readFile path
  debug ("Parsing file " <> path)
  let progParser = runFileParser "" parseProgram progCont
  case progParser of 
    Left err -> throwError (show err)
    Right decls -> return decls

inferCommand :: S.Command -> DriverM T.Command
inferCommand c = do 
  decls <- gets drvEnv
  debug (" Inferring " <> show c <> " with environment " <> show decls)
  debug debugLn 
  (c',ctrs) <- liftErr (runGenCmd decls c)
  debug (" Constraints " <> intercalate "\n" (show <$> ctrs))
  (_,varmap,kndmap) <- liftErr (runSolve ctrs)
  debug (" Substitutions " <> show varmap <> "\n" <> show kndmap)
  return c'

inferTerm :: S.Term -> DriverM T.Term
inferTerm t = do 
  decls <- gets drvEnv 
  debug (" Inferring " <> show t <> " with environment " <> show decls)
  (t',ctrs) <- liftErr (runGenT decls t)
  debug (" Constraints " <> intercalate "\n" (show <$> ctrs))
  (_,varmap,kndmap) <- liftErr (runSolve ctrs)
  debug (" Substitutions " <> show varmap <> "\n" <> show kndmap)
  debug (" Final Type : " <> show (T.getType t'))
  return t'

