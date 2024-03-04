module Eval.Eval where 

import Syntax.Typed.Terms
import Syntax.Typed.Substitution 
import Syntax.Typed.FreeVars
import Common
import Errors

import Control.Monad.Except


newtype EvalM a = MkEvalM { getGenM :: Except Error a }
  deriving newtype (Functor, Applicative, Monad, MonadError Error)


isValue :: Pol -> Term -> Bool
isValue Pos (Var _ _ ) = True 
isValue Pos (Xtor _ args _) = all (isValue Pos) args
isValue Pos (XCase _ _) = True
isValue Pos (Shift  _ _) = True
isValue Pos _ = False 
isValue Neg _ = True

--Co Equivalence <t | + | u> = < u | - | t >
coTrans :: Command -> Command
coTrans (Cut t pol u) = Cut u (flipPol pol) t
coTrans Done = Done

evalOnce :: Command -> EvalM Command
-- beta mu
evalOnce s@(Cut t pol (Mu v c _)) = if isValue pol t then return $ substVar c t v else return s
-- beta shift 
evalOnce (Cut (Shift t _) Pos (Lam v c _)) = return $ substVar c t v
-- beta K
evalOnce s@(Cut (Xtor nm args _) _ (XCase pats _)) = 
  if all (isValue Pos) args then do 
    pt <- findXtor nm pats
    substCase pt args 
  else return s
-- eta K
evalOnce s@(Cut t pol (Xtor nm args ty)) = 
  if isValue pol t 
  then do
    let frV = freshVar 0 (freeVars s)
    case splitArgs args pol of 
      (vals,Just t',ts) -> return $ Cut t Pos (Mu frV (Cut t pol (Xtor nm (vals ++ [t'] ++ ts ) ty)) ty)
      (_, Nothing,_) -> return s
  else return s
  where
    splitArgs :: [Term] -> Pol -> ([Term],Maybe Term,[Term])
    splitArgs [] _ = ([],Nothing,[])
    splitArgs (tm:ts) pl = let (vals,t',rst) = splitArgs ts pl in 
      if isValue pl tm then (t:vals,t',rst)
      else 
        case t' of 
          Nothing -> (vals,Nothing,rst)
          Just t'' ->(vals,Just t,t'':rst)
-- if no other rule applies, swap producer and consumer
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
