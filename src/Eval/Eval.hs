module Eval.Eval where 

import Eval.Definition
import Syntax.Typed.Terms
import Syntax.Typed.Program
import Syntax.Typed.Substitution 
import Syntax.Typed.FreeVars
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
eval c = do
  c' <- evalOnce c
  case c' of 
    Done -> return Done
    _c'' -> if c == c'  || coTrans c == c' then return c else eval c'

evalOnce :: Command -> EvalM Command
evalOnce (Err err) = return $ Err err
-- substitute variables 
evalOnce (Cut (Var v _) pol u) = do
  t <- lookupVar v
  return $ Cut (varBd t) pol u
evalOnce (Cut t pol (Var v _)) = do 
  u <- lookupVar v 
  return $ Cut t pol (varBd u)
-- beta mu
evalOnce (Cut t pol (Mu v c _)) | isValue pol t =   return $ substVar c t v
-- beta shift 
evalOnce (Cut (ShiftPos t _) Pos (ShiftNeg v c _)) = return $ substVar c t v
-- beta K
evalOnce (Cut (Xtor nm args _) _ (XCase pats _)) | all (isValue Pos) args = do 
  pt <- findXtor nm pats
  substCase pt args 
-- eta K
evalOnce s@(Cut t pol (Xtor nm args ty)) | isValue pol t && not (all (isValue pol) args) = do
  let frV = freshVar 0 (freeVars s)
  case splitArgs args pol of 
    (vals,Just t',ts) -> return $ Cut t Pos (Mu frV (Cut t pol (Xtor nm (vals ++ [t'] ++ ts ) ty)) ty)
    (_, Nothing,_) -> return s
  where
    splitArgs :: [Term] -> Pol -> ([Term],Maybe Term,[Term])
    splitArgs [] _ = ([],Nothing,[])
    splitArgs (tm:ts) pl = let (vals,t',rst) = splitArgs ts pl in 
      if isValue pl tm then (t:vals,t',rst)
      else 
        case t' of 
          Nothing -> (vals,Nothing,rst)
          Just t'' ->(vals,Just t,t'':rst)
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
