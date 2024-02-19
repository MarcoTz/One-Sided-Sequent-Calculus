module Driver.Driver where 

import Untyped.Syntax qualified as S
import Typed.Syntax qualified as T
import Typed.Program
import TypeInference.GenerateConstraints
import TypeInference.SolveConstraints

runInfC :: [Decl] -> S.Command -> T.Command
runInfC decls c = 
  case runGenCmd decls c of 
    Left err -> error err
    Right (c',ctrs) -> 
      case runSolve ctrs of 
        Left err -> error err
        Right (_,_varmap,_kndmap) -> c'

runInfT :: [Decl] -> S.Term -> T.Term
runInfT decls t =
  case runGenT decls t of 
    Left err -> error err 
    Right (c',ctrs) -> 
      case runSolve ctrs of 
        Left err -> error err 
        Right (_,_varmap,_kndmap) -> c' 
