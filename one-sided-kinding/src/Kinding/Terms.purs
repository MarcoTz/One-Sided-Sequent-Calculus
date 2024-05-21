module Kinding.Terms (
  kindTerm,
  kindCommand
)where 

import Kinding.Definition (KindM)
import Kinding.Errors (KindError(..))
import Kinding.Types (checkKindType)
import Environment (lookupXtorDecl,lookupXtor)
import Errors (zipWithErrorM)
import Loc (Loc)
import Common (PrdCns(..),EvaluationOrder(..), getKind,Kind, DeclTy(..))
import Syntax.Typed.Terms (Term(..),Pattern(..),Command(..))              as T 
import Syntax.Typed.Types (Ty(..)) as T
import Syntax.Kinded.Terms (Term(..),Pattern(..),Command(..), getPrdCns)  as K
import Syntax.Kinded.Program (DataDecl(..), XtorSig(..))                  as K

import Prelude ((<$>),($),(||),(==),(&&), bind,pure)
import Control.Monad.Except (throwError)
import Data.Traversable (for)
import Data.List(List(..))
import Data.Tuple (Tuple(..))

checkPatterns :: Loc -> List T.Pattern -> KindM (Tuple K.DataDecl (List K.Pattern))
checkPatterns loc Nil = throwError (ErrBadPattern loc)
checkPatterns loc (Cons (T.Pattern pt) pts) = do
  d <- lookupXtorDecl loc pt.ptxt
  pts' <- for pts (\(T.Pattern pt') -> do
     c' <- kindCommand pt'.ptcmd
     pure $ K.Pattern pt'{ptcmd=c'})
  pure (Tuple d pts')

kindTerm :: T.Term -> PrdCns -> EvaluationOrder -> KindM K.Term
kindTerm (T.Var loc v ty) pc eo = do 
  ty' <- checkKindType ty eo
  pure $ K.Var loc pc v ty'
kindTerm (T.Mu loc v c ty) pc eo = do
  c' <- kindCommand c
  ty' <- checkKindType ty eo
  pure $ K.Mu loc pc v c' ty'
kindTerm (T.Xtor loc nm args ty) pc eo = do 
  (K.DataDecl decl) <- lookupXtorDecl loc nm
  let ty' = if (decl.declType == Data && pc==Prd) || (decl.declType == Codata && pc==Cns) then ty else T.TyCo ty
  ty'' <- checkKindType ty' eo
  (K.XtorSig sig) <- lookupXtor loc nm
  let argKnds :: List Kind 
      argKnds = getKind <$> sig.sigArgs
  let kndFun eo' = pure eo'
  argKnds' <- for argKnds kndFun 
  argsZipped <- zipWithErrorM args argKnds' (ErrXtorArity loc nm)
  args' <- for argsZipped (\(Tuple arg knd)  -> kindTerm arg pc knd)
  pure $ K.Xtor loc Prd nm args' ty''
kindTerm (T.XCase loc pts ty) pc eo = do
  (Tuple (K.DataDecl decl) pts') <- checkPatterns loc pts
  let ty' = if (decl.declType == Codata && pc==Prd) || (decl.declType == Data && pc==Cns) then ty else T.TyCo ty
  ty'' <- checkKindType ty' eo
  pure $ K.XCase loc Prd pts' ty''
kindTerm (T.ShiftCBV loc t ty) pc CBV = do
  t' <- kindTerm t pc CBV
  ty' <- checkKindType ty CBV
  pure $ K.ShiftCBV loc (K.getPrdCns t') t' ty'
kindTerm (T.ShiftCBV loc _ _) _ eo = throwError (ErrShift loc eo)
kindTerm (T.ShiftCBN loc t ty) pc CBN = do
  t' <- kindTerm t pc CBN
  ty' <- checkKindType ty CBN 
  pure $ K.ShiftCBN loc (K.getPrdCns t') t' ty'
kindTerm (T.ShiftCBN loc _ _) _ eo = throwError (ErrShift loc eo)

kindCommand :: T.Command -> KindM K.Command 
kindCommand (T.Cut loc t eo u) = do
  t' <- kindTerm t Prd eo
  u' <- kindTerm u Cns eo
  pure $ K.Cut loc t' eo u'
kindCommand (T.Done loc)    = pure $ K.Done loc
kindCommand (T.Err loc msg) = pure $ K.Err loc msg
kindCommand (T.Print loc t) = K.Print loc <$> kindTerm t Prd CBV
