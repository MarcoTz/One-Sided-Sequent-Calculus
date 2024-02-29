module TypeCheck.Definition where 

import Syntax.Typed.Types
import Syntax.Typed.Program
import Common
import Errors 

import Control.Monad
import Control.Monad.State
import Control.Monad.Except 
import Data.Map qualified as M
import Data.List (find)
import Data.Maybe (isJust)

data CheckerState = MkCheckState{ 
  varEnv :: !(M.Map Variable Ty),
  declEnv :: !(M.Map TypeName DataDecl)
} 

initialCheckerState :: CheckerState 
initialCheckerState = MkCheckState M.empty M.empty


newtype CheckM a = CheckM { getCheckM :: StateT CheckerState (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadState CheckerState, MonadError Error)

runGenM :: CheckM a -> Either Error a
runGenM m = case runExcept (runStateT (getCheckM m) initialCheckerState) of
  Left err -> Left err 
  Right (x, _) ->  Right x 

addVar :: Variable -> Ty -> CheckM () 
addVar v ty = modify (\s -> MkCheckState (M.insert v ty (varEnv s)) (declEnv s))

addDecl :: DataDecl -> CheckM () 
addDecl decl = modify (\s -> MkCheckState (varEnv s) (M.insert (declNm decl) decl (declEnv s)))

lookupXtor :: XtorName -> CheckM (Maybe (DataDecl,XtorSig))
lookupXtor xtn = do 
  decls <- gets declEnv 
  let findFun x = xtn == sigName x
  let declFun d = case find findFun (declSig d) of Nothing -> return Nothing; Just xt -> return $ Just (d,xt)
  mdecl <- forM (snd <$> M.toList decls) declFun
  let mdecl' = filter isJust mdecl
  case mdecl' of 
    [] -> return Nothing
    (Nothing:_) -> return Nothing
    (Just res:_) -> return (Just res)

