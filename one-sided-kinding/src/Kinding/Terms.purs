module Kinding.Terms (
  kindTerm,
  kindCommand
)where 

import Kinding.Definition (KindM)
import Kinding.Types (kindType)
import Common (PrdCns(..))
import Syntax.Typed.Terms (Term(..),Pattern(..),Command(..)) as T 
import Syntax.Kinded.Terms (Term(..),Pattern(..),Command(..)) as K

import Prelude ((<$>),($),bind,pure)
import Data.Traversable (for)

kindTerm :: T.Term -> KindM K.Term
kindTerm (T.Var loc v ty) = do 
  ty' <- kindType ty
  pure $ K.Var loc Prd v ty'
kindTerm (T.Mu loc v c ty) = do
  c' <- kindCommand c
  ty' <- kindType ty
  pure $ K.Mu loc Prd v c' ty'
kindTerm (T.Xtor loc nm args ty) = do 
  args' <- for args kindTerm 
  ty' <- kindType ty 
  pure $ K.Xtor loc Prd nm args' ty'
kindTerm (T.XCase loc pts ty) = do
  pts' <- for pts kindPattern 
  ty' <- kindType ty
  pure $ K.XCase loc Prd pts' ty'
kindTerm (T.ShiftCBV loc t ty) = do
  t' <- kindTerm t
  ty' <- kindType ty
  pure $ K.ShiftCBV loc Prd t' ty'
kindTerm (T.ShiftCBN loc t ty) = do
  t' <- kindTerm t 
  ty' <- kindType ty
  pure $ K.ShiftCBN loc Prd t' ty'

kindPattern :: T.Pattern -> KindM K.Pattern 
kindPattern (T.Pattern pt) = do
  c' <- kindCommand pt.ptcmd
  pure $ K.Pattern {ptxt:pt.ptxt, ptv:pt.ptv,ptcmd:c'}


kindCommand :: T.Command -> KindM K.Command 
kindCommand (T.Cut loc t eo u) = do
  t' <- kindTerm t 
  u' <- kindTerm u
  pure $ K.Cut loc t' eo u'
kindCommand (T.Done loc) = pure $ K.Done loc
kindCommand (T.Err loc msg) = pure $ K.Err loc msg
kindCommand (T.Print loc t) = K.Print loc <$> kindTerm t
