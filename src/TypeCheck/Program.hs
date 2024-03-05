module TypeCheck.Program where 

import TypeCheck.Definition
import TypeCheck.Terms
import Syntax.Desugared.Program qualified as D 
import Syntax.Typed.Program     qualified as T 
import Syntax.Typed.Terms       qualified as T 
import Errors

import Control.Monad.Except
import Pretty.Common ()

import Debug.Trace

checkVarDecl :: D.VarDecl -> CheckM T.VarDecl
checkVarDecl (D.MkVar nm (Just ty) t) =  do
  trace ("checking var declaration " <> show nm) $ return ()
  t' <- checkTerm t ty
  return $ T.MkVarDecl nm (T.getType t') t'
checkVarDecl (D.MkVar nm Nothing _) = throwError (ErrMissingType (" Cannot typecheck variable " <> show nm <> " without a type annotation"))
