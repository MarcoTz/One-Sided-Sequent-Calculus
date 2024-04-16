module Desugar.Terms (
  desugarCommand,
  desugarTerm
) where 

import Desugar.Definition(DesugarM,varToXtor,getDesMXtor)
import Desugar.Types (desugarTy)
import Syntax.Parsed.Terms (Term(..),Pattern(..),Command(..)) as P
import Syntax.Desugared.Terms (Term(..),Pattern(..),Command(..)) as D

import Prelude (bind,($),pure,(<$>))
import Data.Traversable (for)
import Data.List (List(..))
import Data.Maybe (Maybe(..))

desugarTerm :: P.Term -> DesugarM D.Term
desugarTerm (P.Var loc v) = do
  let vxt = varToXtor v
  mxt <- getDesMXtor vxt
  case mxt of 
    Nothing -> pure $ D.Var loc v
    Just _ -> pure $ D.Xtor loc vxt Nil 
desugarTerm (P.Mu loc v c) = do 
  c' <- desugarCommand c
  pure $ D.Mu loc v c'
desugarTerm (P.Xtor loc xtn args) = do 
  args' <- for args desugarTerm
  pure $ D.Xtor loc xtn args'
desugarTerm (P.XCase loc pts) = do
  pts' <- for pts desugarPattern
  pure $ D.XCase loc pts'
desugarTerm (P.ShiftCBV loc t) = do
  t' <- desugarTerm t
  pure $ D.ShiftCBV loc t'
desugarTerm (P.ShiftCBN loc t) = do 
  t' <- desugarTerm t
  pure $ D.ShiftCBN loc t'

desugarPattern :: P.Pattern -> DesugarM D.Pattern
desugarPattern (P.Pattern pt) = do 
  c' <- desugarCommand pt.ptcmd
  pure $ D.Pattern {ptxt:pt.ptxt, ptv:pt.ptv, ptcmd:c'}

desugarCommand :: P.Command -> DesugarM D.Command 
desugarCommand (P.Cut loc t pol u) = do 
  t' <- desugarTerm t
  u' <- desugarTerm u 
  pure $ D.Cut loc t' pol u'
desugarCommand (P.CutAnnot loc t ty pol u) = do
  t' <- desugarTerm t
  u' <- desugarTerm u
  ty' <- desugarTy ty 
  pure $ D.CutAnnot loc t' ty' pol u'
desugarCommand (P.Done loc) = pure (D.Done loc)
desugarCommand (P.Err loc str) = pure $ D.Err loc str
desugarCommand (P.Print loc t) = D.Print loc <$> desugarTerm t
desugarCommand (P.PrintAnnot loc t ty) = do 
  t' <- desugarTerm t
  ty' <- desugarTy ty
  pure $ D.PrintAnnot loc t' ty' 
