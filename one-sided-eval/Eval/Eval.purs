module Eval.Eval (
  eval,
  evalWithTrace
) where 

import Eval.Definition
import Eval.Focusing
import Syntax.Kinded.Terms
import Syntax.Kinded.Substitution 
import Common
import Loc
import Environment

import Control.Monad.Except

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
    Done{} -> return newTr
    Err{} -> return newTr
    Print{} -> return newTr
    Cut{} | inTrace tr c' -> throwError (ErrLoop (getLoc c') c)
    Cut{} -> evalFocusedWithTrace c' newTr


evalFocused :: Command -> EvalM Command 
evalFocused c = do
  MkTrace c' _ <- evalFocusedWithTrace c (emptyTrace c)
  return c'

evalOnce :: Command -> EvalM Command
evalOnce (Err loc err) = return $ Err loc err
evalOnce (Done loc) = return $ Done loc
evalOnce (Print loc t) = return $ Print loc t 
evalOnce (Cut loc t pol (Var loc' v _)) = do 
  u <- lookupBody loc v 
  return $ Cut loc t pol (setLoc loc' u)
evalOnce (Cut _ t pol (Mu _ v c _)) | isValue pol t = return $ substituteVariable v t c 
evalOnce (Cut loc (ShiftCBV _ t _) eo u) = return $ Cut loc t eo u
evalOnce (Cut loc (ShiftCBN _ t _) eo u) = return $ Cut loc t eo u
evalOnce (Cut loc (Xtor _ nm args _) pol (XCase _ pats _)) | all (isValue pol) args = do 
  pt <- findXtor loc nm pats
  substCase loc pt args 
evalOnce cmd = return $ coTrans cmd

substCase :: Loc -> Pattern -> [Term] -> EvalM Command
substCase _ MkPattern{ptxt=_, ptv=[], ptcmd=cmd} []  = return cmd
substCase loc MkPattern{ptxt=xt, ptv=(v:vs), ptcmd=cmd} (t:ts) = 
  let newcmd = substituteVariable v t cmd
  in substCase loc MkPattern{ptxt=xt,ptv=vs,ptcmd=newcmd} ts
substCase loc (MkPattern xt [] _) (_:_) = throwError (ErrXtorArity loc xt) 
substCase loc (MkPattern xt (_:_) _) [] = throwError (ErrXtorArity loc xt) 

findXtor :: Loc -> Xtorname -> [Pattern] -> EvalM Pattern
findXtor loc xt [] = throwError (ErrMissingPt loc xt) 
findXtor loc xt (pt:pts) = if ptxt pt == xt then return pt else findXtor loc xt pts
