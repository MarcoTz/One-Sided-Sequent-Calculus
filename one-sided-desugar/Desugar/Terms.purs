module Desugar.Terms (
  desugarCommand,
  desugarTerm
) where 

import Desugar.Definition
import Desugar.Types
import Syntax.Parsed.Terms    qualified as P
import Syntax.Desugared.Terms qualified as D 

import Control.Monad

desugarTerm :: P.Term -> DesugarM D.Term
desugarTerm (P.Var loc v) = do
  let vxt = varToXtor v
  mxt <- getDesMXtor vxt
  case mxt of 
    Nothing -> return $ D.Var loc v
    Just _ -> return $ D.Xtor loc vxt [] 
desugarTerm (P.Mu loc v c) = do 
  c' <- desugarCommand c
  return $ D.Mu loc v c'
desugarTerm (P.Xtor loc xtn args) = do 
  args' <- forM args desugarTerm
  return $ D.Xtor loc xtn args'
desugarTerm (P.XCase loc pts) = do
  pts' <- forM pts desugarPattern
  return $ D.XCase loc pts'
desugarTerm (P.ShiftCBV loc t) = do
  t' <- desugarTerm t
  return $ D.ShiftCBV loc t'
desugarTerm (P.ShiftCBN loc t) = do 
  t' <- desugarTerm t
  return $ D.ShiftCBN loc t'

desugarPattern :: P.Pattern -> DesugarM D.Pattern
desugarPattern (P.MkPattern xtn vars c) = do 
  c' <- desugarCommand c
  return $ D.MkPattern xtn vars c'

desugarCommand :: P.Command -> DesugarM D.Command 
desugarCommand (P.Cut loc t pol u) = do 
  t' <- desugarTerm t
  u' <- desugarTerm u 
  return $ D.Cut loc t' pol u'
desugarCommand (P.CutAnnot loc t ty pol u) = do
  t' <- desugarTerm t
  u' <- desugarTerm u
  ty' <- desugarTy ty 
  return $ D.CutAnnot loc t' ty' pol u'
desugarCommand (P.Done loc) = return (D.Done loc)
desugarCommand (P.Err loc str) = return $ D.Err loc str
desugarCommand (P.Print loc t) = D.Print loc <$> desugarTerm t
desugarCommand (P.PrintAnnot loc t ty) = do 
  t' <- desugarTerm t
  ty' <- desugarTy ty
  return $ D.PrintAnnot loc t' ty' 
