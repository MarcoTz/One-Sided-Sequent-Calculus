module TypeInference.GenerateConstraints.Program where 

import TypeInference.GenerateConstraints.Definition
import TypeInference.GenerateConstraints.Terms
import Syntax.Desugared.Program qualified as D 
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Program qualified as T 
import Syntax.Typed.Types qualified as T
import Syntax.Typed.Terms qualified as T

import Control.Monad 

genConstraintsProgram :: D.Program -> GenM T.Program
genConstraintsProgram (D.MkProgram decls vars) = do
  decls' <- forM decls genConstraintsDecl 
  vars' <- forM vars genConstraintsVar
  return $ T.MkProgram decls' vars'

genConstraintsDecl :: D.DataDecl -> GenM T.DataDecl
genConstraintsDecl (D.MkDataDecl tyn args pol xtors) = do 
  xtors' <- forM xtors genConstraintsXtorSig 
  return $ T.MkDataDecl tyn args pol xtors'

genConstraintsXtorSig :: D.XtorSig -> GenM T.XtorSig
genConstraintsXtorSig (D.MkXtorSig nm args) = do 
  args' <- forM args genConstraintsTy
  return $ T.MkXtorSig nm args'

genConstraintsTy :: D.Ty -> GenM T.Ty
genConstraintsTy (D.TyVar v knd) = return $ T.TyVar v knd
genConstraintsTy (D.TyDecl tyn args knd) = do 
  args' <- forM args genConstraintsTy
  return $ T.TyDecl tyn args' knd

genConstraintsVar :: D.VarDecl -> GenM T.VarDecl
genConstraintsVar (D.MkVarDecl v t) = do
  t' <- genConstraintsTerm t
  return $ T.MkVarDecl v (T.getType t') t'
