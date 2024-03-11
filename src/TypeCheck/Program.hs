module TypeCheck.Program where 

import TypeCheck.Definition
import TypeCheck.Terms
import TypeCheck.Types
import Syntax.Desugared.Program qualified as D 
import Syntax.Desugared.Terms   qualified as D 
import Syntax.Typed.Program     qualified as T 
import Syntax.Typed.Terms       qualified as T
import Errors
import Pretty.Common ()

import Control.Monad.Except
import Control.Monad

checkVarDecl :: D.VarDecl -> CheckM T.VarDecl
checkVarDecl (D.MkVar nm vars (Just polty) t) =  do
  ty <- checkPolTy polty
  vars' <- forM vars checkVar 
  forM_ vars' (uncurry addVarPol)
  t' <- checkTerm t ty
  return $ T.MkVar nm vars' (T.getType t') t'
  where 
    checkVar :: D.MTypedVar -> CheckM T.TypedVar
    checkVar (v,Nothing)  = throwError (ErrMissingType ("No type for variable " <> show v <> "checkVarDecl"))
    checkVar (v,Just pty) = (v,) <$> checkPolTy pty

checkVarDecl (D.MkVar nm _ Nothing _) = throwError (ErrMissingType (" Cannot typecheck variable " <> show nm <> " without a type annotation"))
