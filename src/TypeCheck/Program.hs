module TypeCheck.Program where 

import TypeCheck.Definition
import TypeCheck.Terms
import TypeCheck.Types
import Syntax.Desugared.Program qualified as D 
import Syntax.Desugared.Types   qualified as D
import Syntax.Typed.Program     qualified as T 
import Syntax.Typed.Terms       qualified as T
import Syntax.Typed.Types       qualified as T
import Errors
import Common

import Control.Monad.Except
import Control.Monad
import Pretty.Common ()

checkVarDecl :: D.VarDecl -> CheckM T.VarDecl
checkVarDecl (D.MkVar nm vars (Just ty) t) =  do
  t' <- checkTerm t ty
  vars' <- forM vars checkVarTy 
  return $ T.MkVar nm vars' (T.getType t') t'
  where
    checkVarTy :: (Variable,Maybe D.Ty) -> CheckM (Variable, T.Ty)
    checkVarTy (v, Nothing) = throwError (ErrMissingType ("Cannot typecheck, variable " <> show v <> " has no type"))
    checkVarTy (v,Just ty') = do 
      ty'' <- checkType ty' Nothing
      return (v,ty'')
checkVarDecl (D.MkVar nm _ Nothing _) = throwError (ErrMissingType (" Cannot typecheck variable " <> show nm <> " without a type annotation"))
