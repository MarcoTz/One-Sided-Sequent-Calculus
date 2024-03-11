module Desugar.Terms where 

import Desugar.Definition
import Desugar.Types
import Syntax.Parsed.Terms    qualified as P
import Syntax.Desugared.Terms qualified as D 

import Control.Monad

desugarTerm :: P.Term -> DesugarM D.Term
desugarTerm (P.Var v) = do
  let vxt = varToXtor v
  mxt <- getMXtor vxt
  case mxt of 
    Nothing -> return $ D.Var v
    Just _ -> return $ D.Xtor vxt [] 
desugarTerm (P.Mu v c) = do 
  c' <- desugarCommand c
  return $ D.Mu v c'
desugarTerm (P.Xtor xtn args) = do 
  args' <- forM args desugarTerm
  return $ D.Xtor xtn args'
desugarTerm (P.XCase pts) = do
  pts' <- forM pts desugarPattern
  return $ D.XCase pts'
desugarTerm (P.ShiftPos t) = do
  t' <- desugarTerm t
  return $ D.ShiftPos t'
desugarTerm (P.ShiftNeg v c) = do 
  c' <- desugarCommand c
  return $ D.ShiftNeg v c'

desugarPattern :: P.Pattern -> DesugarM D.Pattern
desugarPattern (P.MkPattern xtn vars c) = do 
  c' <- desugarCommand c
  return $ D.MkPattern xtn vars c'

desugarCommand :: P.Command -> DesugarM D.Command 
desugarCommand (P.Cut t pol u) = do 
  t' <- desugarTerm t
  u' <- desugarTerm u 
  return $ D.Cut t' pol u'
desugarCommand (P.CutAnnot t ty pol u) = do
  t' <- desugarTerm t
  u' <- desugarTerm u
  ty' <- desugarTy ty 
  return $ D.CutAnnot t' ty' pol u'
desugarCommand P.Done = return D.Done
