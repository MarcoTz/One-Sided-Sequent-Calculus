module Eval.Eval (
  eval
) where 

import Eval.Definition
import Eval.Focusing
import Syntax.Typed.Terms
import Syntax.Typed.Substitution 
import Common
import Environment
import Errors

import Control.Monad.Except

--Co Equivalence <t | + | u> = < u | - | t >
coTrans :: Command -> Command
coTrans (Cut t pol u) = Cut u (flipPol pol) t
coTrans Done = Done
coTrans (Err err) = Err err

eval :: Command -> EvalM Command 
eval c = let c' = focus c in evalFocused c'

evalFocused :: Command -> EvalM Command 
evalFocused c = do
  c' <- evalOnce c
  case c' of 
    Done -> return Done
    (Err err) -> return (Err err)
    _c'' -> evalFocused c'

evalOnce :: Command -> EvalM Command
evalOnce (Err err) = return $ Err err
evalOnce Done = return Done
-- substitute variables 
evalOnce (Cut (Var v _) pol u) = do
  t <- lookupBody v
  return $ Cut t pol u
evalOnce (Cut t pol (Var v _)) = do 
  u <- lookupBody v 
  return $ Cut t pol u 
-- beta mu
evalOnce (Cut t pol (Mu v c _)) | isValue pol t = return $ substVar c t v
-- beta shift 
evalOnce (Cut (ShiftPos t _) Pos (ShiftNeg v c _)) = return $ substVar c t v
-- beta K

evalOnce (Cut (Xtor nm args _) pol (XCase pats _)) | all (isValue pol) args = do 
  pt <- findXtor nm pats
  substCase pt args 
evalOnce cmd = evalOnce (coTrans cmd) 

substCase :: Pattern -> [Term] -> EvalM Command
--MkPattern{ptxt :: !String, ptv :: ![Variable], ptcmd :: !Command}
substCase MkPattern{ptxt=_, ptv=[], ptcmd=cmd} []  = return cmd
substCase MkPattern{ptxt=xt, ptv=(v:vs), ptcmd=cmd} (t:ts) = 
  let newcmd = substVar cmd t v 
  in substCase MkPattern{ptxt=xt,ptv=vs,ptcmd=newcmd} ts
substCase (MkPattern xt [] _) (_:_) = throwError (ErrXtorArity xt "substCase (Eval)") 
substCase (MkPattern xt (_:_) _) [] = throwError (ErrXtorArity xt "substCase (Eval)") 

findXtor :: XtorName -> [Pattern] -> EvalM Pattern
findXtor xt [] = throwError (ErrMissingXtorPt xt "findXtor (Eval)") 
findXtor xt (pt:pts) = if ptxt pt == xt then return pt else findXtor xt pts
