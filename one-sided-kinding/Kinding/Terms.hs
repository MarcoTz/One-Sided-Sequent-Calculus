module Kinding.Terms where 

import Kinding.Definition
import Kinding.Types
import Syntax.Typed.Terms qualified as T
import Syntax.Kinded.Terms qualified as K

import Control.Monad

kindTerm :: T.Term -> KindM K.Term
kindTerm (T.Var loc v ty) = K.Var loc v <$> kindType ty
kindTerm (T.Mu loc v c ty) = do
  c' <- kindCommand c
  ty' <- kindType ty
  return $ K.Mu loc v c' ty'
kindTerm (T.Xtor loc nm args ty) = do 
  args' <- forM args kindTerm 
  ty' <- kindType ty 
  return $ K.Xtor loc nm args' ty'
kindTerm (T.XCase loc pts ty) = do
  pts' <- forM pts kindPattern 
  ty' <- kindType ty
  return $ K.XCase loc pts' ty'
kindTerm (T.ShiftCBV loc t ty) = do
  t' <- kindTerm t
  ty' <- kindType ty
  return $ K.ShiftCBV loc t' ty'
kindTerm (T.ShiftCBN loc t ty) = do
  t' <- kindTerm t 
  ty' <- kindType ty
  return $ K.ShiftCBN loc t' ty'

kindPattern :: T.Pattern -> KindM K.Pattern 
kindPattern (T.MkPattern nm vars c) = K.MkPattern nm vars <$> kindCommand c


kindCommand :: T.Command -> KindM K.Command 
kindCommand (T.Cut loc t eo u) = do
  t' <- kindTerm t 
  u' <- kindTerm u
  return $ K.Cut loc t' eo u'
kindCommand (T.Done loc) = return $ K.Done loc
kindCommand (T.Err loc msg) = return $ K.Err loc msg
kindCommand (T.Print loc t) = K.Print loc <$> kindTerm t
