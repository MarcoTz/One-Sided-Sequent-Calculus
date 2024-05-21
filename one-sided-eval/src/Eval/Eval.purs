module Eval.Eval (
  eval,
  evalWithTrace
) where 

import Loc (getLoc,setLoc)
import Common (shiftEvalOrder, Variable)
import Errors (zipWithErrorM)
import Environment (lookupBody)
import Syntax.Kinded.Terms (Command(..), Term(..), isValue, Pattern(..), getType, getPrdCns)
import Syntax.Kinded.Substitution (substituteVariable,substVars)
import FreeVars.FreeVariables (freshVar) 
import Eval.Definition (EvalM, EvalTrace(..),emptyTrace, EvalError(..), appendTrace)

import Prelude (bind,pure,($), (==),(/=))
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Map (fromFoldable)
import Control.Monad (when)
import Control.Monad.Except (throwError)


evalWithTrace :: Command -> EvalTrace -> EvalM EvalTrace 
evalWithTrace c tr = do 
  c' <- evalOnce c 
  let newTr = appendTrace tr c'
  case c' of 
    (Done _)  -> pure newTr
    (Err _ _) -> pure newTr
    (Print _ _) -> pure newTr
    (Cut _ _ _ _) -> do
       _ <- when (c == c') $ throwError (ErrLoop (getLoc c) c)
       evalWithTrace c' newTr


eval :: Command -> EvalM Command 
eval c = do
  MkTrace c' _ <- evalWithTrace c (emptyTrace c)
  pure c'

evalOnce :: Command -> EvalM Command
evalOnce (Err loc err) = pure $ Err loc err
evalOnce (Done loc)    = pure $ Done loc
evalOnce (Print loc t) = pure $ Print loc t 

evalOnce (Cut loc (ShiftCBV _ _ t _) eo u) = pure $ Cut loc t eo u
evalOnce (Cut loc (ShiftCBN _ _ t _) eo u) = pure $ Cut loc t eo u
evalOnce (Cut loc t eo (ShiftCBV _ _ u _)) = pure $ Cut loc t eo u
evalOnce (Cut loc t eo (ShiftCBN _ _ u _)) = pure $ Cut loc t eo u

evalOnce (Cut loc (Var loc' _ v _) eo u) = do 
  t <- lookupBody loc v 
  pure $ Cut loc (setLoc loc' t) eo u 
evalOnce (Cut loc t eo (Var loc' _ v _)) = do
  u <- lookupBody loc v 
  pure $ Cut loc t eo (setLoc loc' u)

evalOnce (Cut _ t _ (Mu _ _ v c _)) = pure $ substituteVariable v t c 
evalOnce (Cut _ (Mu _ _ v c _) _ u) = pure $ substituteVariable v u c

evalOnce (Cut loc (Xtor loc1 pc nm args ty) eo xc@(XCase _ _ pats _)) = do
  let (Tuple args' mv) = evalArgs args
  case mv of 
      -- all arguments are values 
      Nothing -> do
        substCase pats
      Just (Tuple var t') -> do
        let xtt = Xtor loc1 pc nm args' ty
        pure $ Cut loc (Mu loc1 pc var (Cut loc1 xtt eo t') (getType t')) eo xc
  where 
    evalArgs :: List Term -> Tuple (List Term) (Maybe (Tuple Variable Term))
    evalArgs Nil = Tuple Nil Nothing
    evalArgs (Cons t1 ts) | isValue eo t1 = let Tuple ts' mv = evalArgs ts in Tuple (Cons t1 ts') mv
    evalArgs (Cons t1 ts) = do
      let frv = freshVar ts 
      let newArgs = Cons (Var (getLoc t1) (getPrdCns t1) frv (getType t1)) ts
      Tuple newArgs (Just (Tuple frv t1))

    substCase :: List Pattern -> EvalM Command
    substCase Nil = throwError (ErrMissingPt loc nm) 
    substCase (Cons (Pattern pt) pts) | pt.ptxt /= nm = substCase pts
    substCase (Cons (Pattern pt) _) = do 
       argsZipped <- zipWithErrorM pt.ptv args (ErrMissingPt loc nm)
       let varmap = fromFoldable argsZipped
       pure $ substVars varmap pt.ptcmd

evalOnce (Cut loc xc@(XCase _ _ _ _) eo xt@(Xtor _ _ _ _ _)) = pure (Cut loc xt (shiftEvalOrder eo) xc)
evalOnce c@(Cut loc (XCase _ _ _ _) _ (XCase _ _ _ _)) = throwError (ErrTwoCase loc c)
evalOnce c@(Cut loc (Xtor _ _ _ _ _) _ (Xtor _ _ _ _ _)) = throwError (ErrTwoXtor loc c)
