module Eval.Eval (
  eval,
  evalWithTrace
) where 

import Loc (Loc,getLoc,setLoc)
import Common (shiftEvalOrder, Xtorname)
import Environment (lookupBody)
import Syntax.Kinded.Terms (Command(..), Term(..), isValue, Pattern(..))
import Syntax.Kinded.Substitution (substituteVariable)
import Eval.Definition (EvalM, EvalTrace(..),emptyTrace, EvalError(..), appendTrace, inTrace)
import Eval.Focusing (focus)

import Prelude (bind,pure,($),identity,(<$>), (==))
import Data.List (List(..),elem,null,filter)
import Control.Monad.Except (throwError)

--Co Equivalence <t | + | u> = < u | - | t >
coTrans :: Command -> Command
coTrans (Cut loc t eo u) = Cut loc u (shiftEvalOrder eo) t
coTrans (Done loc) = Done loc
coTrans (Err loc err) = Err loc err
coTrans (Print loc t) = Print loc t

eval :: Command -> EvalM Command 
eval c = let c' = focus c in evalFocused c'

evalWithTrace :: Command -> EvalM EvalTrace 
evalWithTrace c = let c' = focus c in evalFocusedWithTrace c' (emptyTrace c')

evalFocusedWithTrace :: Command -> EvalTrace -> EvalM EvalTrace 
evalFocusedWithTrace c (MkTrace _ tr) | c `elem` tr = throwError (ErrLoop (getLoc c) c)
evalFocusedWithTrace c tr = do 
  c' <- evalOnce c 
  let newTr = appendTrace tr c'
  case c' of 
    (Done _)  -> pure newTr
    (Err _ _) -> pure newTr
    (Print _ _) -> pure newTr
    (Cut _ _ _ _) | inTrace tr c' -> throwError (ErrLoop (getLoc c') c)
    (Cut _ _ _ _) -> evalFocusedWithTrace c' newTr


evalFocused :: Command -> EvalM Command 
evalFocused c = do
  MkTrace c' _ <- evalFocusedWithTrace c (emptyTrace c)
  pure c'

evalOnce :: Command -> EvalM Command
evalOnce (Err loc err) = pure $ Err loc err
evalOnce (Done loc) = pure $ Done loc
evalOnce (Print loc t) = pure $ Print loc t 
evalOnce (Cut loc t pol (Var loc' v _)) = do 
  u <- lookupBody loc v 
  pure $ Cut loc t pol (setLoc loc' u)
evalOnce (Cut _ t pol (Mu _ v c _)) | isValue pol t = pure $ substituteVariable v t c 
evalOnce (Cut loc (ShiftCBV _ t _) eo u) = pure $ Cut loc t eo u
evalOnce (Cut loc (ShiftCBN _ t _) eo u) = pure $ Cut loc t eo u
evalOnce (Cut loc (Xtor _ nm args _) pol (XCase _ pats _)) | null (filter identity ((isValue pol) <$> args)) = do 
  pt <- findXtor loc nm pats
  substCase loc pt args 
evalOnce cmd = pure $ coTrans cmd

substCase :: Loc -> Pattern -> List Term -> EvalM Command
substCase _ (Pattern{ptxt:_, ptv:Nil, ptcmd:cmd}) Nil = pure cmd
substCase loc (Pattern{ptxt:xt, ptv:(Cons v vs), ptcmd:cmd}) (Cons t ts) = 
  let newcmd = substituteVariable v t cmd
  in substCase loc (Pattern{ptxt:xt,ptv:vs,ptcmd:newcmd}) ts
substCase loc (Pattern{ptxt:xt,ptv:Nil,ptcmd:_}) (Cons _ _) = throwError (ErrXtorArity loc xt) 
substCase loc (Pattern{ptxt:xt,ptv:Cons _ _,ptcmd:_}) Nil = throwError (ErrXtorArity loc xt) 

findXtor :: Loc -> Xtorname -> List Pattern -> EvalM Pattern
findXtor loc xt Nil = throwError (ErrMissingPt loc xt) 
findXtor loc xt (Cons (Pattern pt) pts) = if pt.ptxt == xt then pure (Pattern pt) else findXtor loc xt pts
