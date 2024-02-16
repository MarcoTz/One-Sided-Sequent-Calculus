module Eval where 

import Syntax 
import Substitution 

isValue :: Pol -> Term -> Bool
isValue Pos (Var _) = True 
isValue Pos (Xtor _ args) = all (isValue Pos) args
isValue Pos (XCase _) = True
isValue Pos (Shift _) = True
isValue Pos _ = False 
isValue Neg _ = True

--Co Equivalence <t | + | u> = < u | - | t >
coTrans :: Command -> Command
coTrans (Cut t pol u) = Cut u (flipPol pol) t

evalOnce :: Command -> Command
-- beta mu
evalOnce s@(Cut t pol (Mu v c)) = if isValue pol t then substVar c t v else s
-- beta shift 
evalOnce (Cut (Shift t) Pos (Lam v c)) = substVar c t v
-- beta K
evalOnce s@(Cut (Xtor nm args) _ (XCase pats)) = 
  if all (isValue Pos) args then
    case findXtor pats nm of 
      Nothing -> s 
      Just MkPattern{ptxt=_,ptv=vars,ptcmd=cmd} -> 
        if length vars == length vars then foldr (\(v,t) st -> substVar st t v ) cmd (zip vars args)
        else s
  else s
  where 
    findXtor :: [Pattern] -> String -> Maybe Pattern
    findXtor [] _ = Nothing
    findXtor (pt:pts) xt = if ptxt pt == xt then Just pt else findXtor pts xt
-- eta K
evalOnce s@(Cut t pol (Xtor nm args)) = 
  if isValue pol t 
  then do
    let frV = freshVar 0 (freeVars s)
    case splitArgs args pol of 
      (vals,Just t',ts) -> Cut t Pos (Mu frV (Cut t pol (Xtor nm (vals ++ [t'] ++ ts ))))
      (_, Nothing,_) -> s
  else s
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
