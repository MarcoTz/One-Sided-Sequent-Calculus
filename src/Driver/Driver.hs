module Driver.Driver where 

import Driver.Definition
import Untyped.Syntax qualified as S
import Untyped.Program qualified as S
import Typed.Syntax qualified as T
import Parser.Definition
import Parser.Program
import TypeInference.GenerateConstraints
import TypeInference.SolveConstraints
import Pretty.Terms ()
import Pretty.Program ()
import Pretty.TypeInference ()

import Control.Monad.State
import Control.Monad.Except
import Data.List (intercalate)
import Data.Text.IO qualified as T


checkDecls :: [S.DataDecl] -> DriverM ()
checkDecls decls = do 
  let declNms = S.declNm <$> decls 
  let declXtors = S.sigName <$> concatMap S.declSig decls
  case (checkDups declNms, checkDups declXtors) of 
    (Nothing,Nothing) -> return ()
    (Just tn,_) -> throwError ("Type " <> tn <> " declared multiple times")
    (_,Just xtn) -> throwError ("Xtor " <> xtn <> " declared multiple times")
  where 
    checkDups :: Eq a => [a] -> Maybe a 
    checkDups [] = Nothing
    checkDups (tn:tns) = if tn `elem` tns then Just tn else checkDups tns

inferProgram :: FilePath -> DriverM () 
inferProgram path = do 
  progCont <- liftIO $ T.readFile path
  debug ("Parsing file " <> path)
  let progParser = runFileParser "" parseProgram progCont
  prog <- liftErr progParser
  debug ("Successfully parsed " <> path)
  debug ("parsed declarations " <> show (S.pgDat prog))
  debug ("parsed terms " <> show (S.pgTm prog))
  debug "Checking well-formed program"
  checkDecls (S.pgDat prog)

inferCommand :: S.Command -> DriverM T.Command
inferCommand c = do 
  decls <- gets drvEnv
  debug (" Inferring " <> show c <> " with environment " <> show decls)
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

