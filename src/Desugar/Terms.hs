module Desugar.Terms where 

import Desugar.Definition
import Syntax.Parsed.Terms qualified as P
import Syntax.Desugared.Terms qualified as D 
import Syntax.Desugared.Program qualified as D

import Control.Monad
import Control.Monad.State

desugarTerm :: P.Term -> DesugarM D.Term
desugarTerm (P.Var v) = do
  decls <- gets desDecls 
  let declXts = D.sigName <$> concatMap D.declSig decls
  if v `elem`declXts then return $ D.Xtor v [] else return $ D.Var v
desugarTerm (P.Mu v c) = do 
  c' <- desugarCommand c
  return $ D.Mu v c'
desugarTerm (P.Xtor xtn args) = do 
  args' <- forM args desugarTerm
  return $ D.Xtor xtn args'
desugarTerm (P.XCase pts) = do
  pts' <- forM pts desugarPattern
  return $ D.XCase pts'
desugarTerm (P.Shift t) = do
  t' <- desugarTerm t
  return $ D.Shift t'
desugarTerm (P.Lam v c) = do 
  c' <- desugarCommand c
  return $ D.Lam v c'

desugarPattern :: P.Pattern -> DesugarM D.Pattern
desugarPattern (P.MkPattern xtn vars c) = do 
  c' <- desugarCommand c
  return $ D.MkPattern xtn vars c'

desugarCommand :: P.Command -> DesugarM D.Command 
desugarCommand (P.Cut t pol u) = do 
  t' <- desugarTerm t
  u' <- desugarTerm u 
  return $ D.Cut t' pol u'
desugarCommand P.Done = return D.Done
