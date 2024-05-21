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

import Prelude ((<$>),($), bind,pure)
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
  pt' <- (do 
     c' <- kindCommand pt.ptcmd
     pure $ K.Pattern pt{ptcmd=c'})
  pure (Tuple d (Cons pt' pts'))

data TermTy = XTor | XCase 
getCo :: TermTy -> DeclTy -> PrdCns -> T.Ty -> Tuple T.Ty PrdCns
getCo XTor  Data   Cns    ty    = Tuple (T.TyCo ty) Cns 
getCo XTor  Codata Prd    ty    = Tuple (T.TyCo ty) Prd
getCo XCase Data   Prd    ty    = Tuple (T.TyCo ty) Prd
getCo XCase Codata Cns    ty    = Tuple (T.TyCo ty) Cns 
getCo XTor  Data   PrdCns ty    = Tuple ty          Prd
getCo XCase Data   PrdCns ty    = Tuple ty          Cns 
getCo XTor  Codata PrdCns ty    = Tuple ty          Cns
getCo XCase Codata PrdCns ty    = Tuple ty          Prd
getCo _     _      pc     ty    = Tuple ty          pc

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
  let Tuple ty' pc' = getCo XTor decl.declType pc ty  
  ty'' <- checkKindType ty' eo
  (K.XtorSig sig) <- lookupXtor loc nm
  let argKnds :: List Kind 
      argKnds = getKind <$> sig.sigArgs
  let kndFun eo' = pure eo'
  argKnds' <- for argKnds kndFun 
  argsZipped <- zipWithErrorM args argKnds' (ErrXtorArity loc nm)
  args' <- for argsZipped (\(Tuple arg knd)  -> kindTerm arg pc knd)
  pure $ K.Xtor loc pc' nm args' ty''

kindTerm (T.XCase loc pts ty) pc eo = do
  (Tuple (K.DataDecl decl) pts') <- checkPatterns loc pts
  let Tuple ty' pc' = getCo XCase decl.declType pc ty
  ty'' <- checkKindType ty' eo
  pure $ K.XCase loc pc' pts' ty''

kindTerm (T.ShiftCBV loc _ _) _ CBN = throwError (ErrShift loc CBN)
kindTerm (T.ShiftCBV loc t ty) pc _ = do
  t' <- kindTerm t pc CBV
  ty' <- checkKindType ty CBV
  pure $ K.ShiftCBV loc (K.getPrdCns t') t' ty'

kindTerm (T.ShiftCBN loc _ _) _ CBV = throwError (ErrShift loc CBV)
kindTerm (T.ShiftCBN loc t ty) pc _ = do
  t' <- kindTerm t pc CBN
  ty' <- checkKindType ty CBN 
  pure $ K.ShiftCBN loc (K.getPrdCns t') t' ty'

kindCommand :: T.Command -> KindM K.Command 
kindCommand (T.Cut loc t eo u) = do
  t' <- kindTerm t Prd eo
  u' <- kindTerm u Cns eo
  pure $ K.Cut loc t' eo u'
kindCommand (T.Done loc)    = pure $ K.Done loc
kindCommand (T.Err loc msg) = pure $ K.Err loc msg
kindCommand (T.Print loc t) = K.Print loc <$> kindTerm t Prd CBV
