module Desugar.Terms (
  desugarCommand,
  desugarTerm
) where 

import Common (Xtorname(..),EvaluationOrder(..))
import Desugar.Definition(DesugarM,varToXtor,getDesMXtor)
import Desugar.Errors (DesugarError(..))
import Desugar.Types (desugarTy)
import Syntax.Parsed.Terms (Term(..),Pattern(..),Command(..)) as P
import Syntax.Desugared.Terms (Term(..),Pattern(..),Command(..)) as D
import FreeVars.FreeVariables (freshVar)

import Prelude (bind,($),pure,(<$>))
import Data.Traversable (for)
import Data.List (List(..),uncons)
import Data.Maybe (Maybe(..))
import Control.Monad.Except (throwError)

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
desugarTerm t@(P.App loc t1 t2) = do
  t1' <- desugarTerm t1 
  t2' <- desugarTerm t2
  let v = freshVar t
  let args = Cons t2' (Cons (D.Var loc v) Nil)
  let cut = D.Cut loc t1' CBV (D.Xtor loc (Xtorname "Ap") args) 
  pure $ D.Mu loc v cut
desugarTerm t@(P.Lam loc v t') = do
  let v' = freshVar t
  let ptVars = Cons v (Cons v' Nil)
  t'' <- desugarTerm t'
  let cut = D.Cut loc t'' CBV (D.Var loc v')
  let pt = D.Pattern {ptxt:Xtorname "Ap", ptv:ptVars, ptcmd:cut }
  pure $ D.XCase loc (Cons pt Nil) 
desugarTerm t@(P.Seq loc t1 t2) = do 
  let v = freshVar t
  desugarTerm $ P.App loc (P.Lam loc v t2) t1
desugarTerm (P.Tup loc ts) = case uncons ts of 
  Nothing -> throwError (ErrEmptyPair loc)
  Just {head:_,tail:Nil} -> throwError (ErrEmptyPair loc)
  Just {head:t1,tail:Cons t2 Nil} -> do
    t1' <- desugarTerm t1 
    t2' <- desugarTerm t2
    pure $ D.Xtor loc (Xtorname "Tup") (Cons t1' (Cons t2' Nil))
  Just {head:t1,tail:ts'} -> do 
    t1' <- desugarTerm t1
    pairRest <- desugarTerm (P.Tup loc ts') 
    pure $ D.Xtor loc (Xtorname "Tup") (Cons t1' (Cons pairRest Nil))
desugarTerm (P.Lst loc ts) = case ts of 
  Nil -> pure $ D.Xtor loc (Xtorname "Nil") Nil
  Cons t1 ts' -> do
     t1' <- desugarTerm t1
     listRest <- desugarTerm (P.Lst loc ts')
     pure $ D.Xtor loc (Xtorname "Cons") (Cons t1' (Cons listRest Nil))

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
