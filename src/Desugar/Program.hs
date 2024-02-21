module Desugar.Program where 

import Control.Monad 

import Desugar.Definition
import Desugar.Terms
import Syntax.Parsed.Program qualified as P
import Syntax.Desugared.Program qualified as D

desugarProgram :: P.Program -> DesugarM D.Program
desugarProgram (P.MkProgram decls vars) = do 
  decls' <- forM decls desugarDecl
  vars' <- forM vars desugarVar
  return $ D.MkProgram decls' vars'


desugarDecl :: P.DataDecl -> DesugarM D.DataDecl
desugarDecl (P.MkDataDecl tyn tyargs  pol sigs)= do 
  sigs' <- forM sigs desugarXtorSig
  return $ D.MkDataDecl tyn tyargs pol sigs'

desugarVar :: P.VarDecl -> DesugarM D.VarDecl
desugarVar (P.MkVarDecl v t) = do 
  t' <- desugarTerm t
  return $ D.MkVarDecl v t'

desugarXtorSig :: P.XtorSig -> DesugarM D.XtorSig
desugarXtorSig (P.MkXtorSig xtn args) = do
  args' <- forM args desugarTy
  return (D.MkXtorSig xtn args')

desugarTy :: P.Ty -> DesugarM D.Ty
desugarTy (P.TyVar v) = return $ D.TyVar v
desugarTy (P.TyDecl tyn args) = do 
  args' <- forM args desugarTy 
  return $ D.TyDecl tyn args'

