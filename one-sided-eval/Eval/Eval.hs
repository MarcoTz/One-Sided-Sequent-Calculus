module Eval.Eval (
  eval,
  evalWithTrace
) where 

import Eval.Definition
import Eval.Focusing
import Syntax.Typed.Terms
import Syntax.Typed.Substitution 
import Common
import Loc
import Environment

import Control.Monad.Except
import Data.Bifunctor (second)

--Co Equivalence <t | + | u> = < u | - | t >
coTrans :: Command -> Command
coTrans (Cut loc t eo u) = Cut loc u (shiftEvalOrder eo) t
coTrans (Done loc) = Done loc
coTrans (Err loc err) = Err loc err
coTrans (Print loc t) = Print loc t

eval :: Command -> EvalM Command 
eval c = let c' = focus c in evalFocused c'

evalWithTrace :: Command -> EvalM EvalTrace 
evalWithTrace c = let c' = focus c in uncurry MkTrace <$> evalFocusedWithTrace c'

evalFocusedWithTrace :: Command -> EvalM (Command,[Command])
evalFocusedWithTrace c = do 
  c' <- evalOnce c
  let tr = [c,c']
  case c' of 
    (Done _) -> return (c',tr)
    (Err _ _) -> return (c',tr) 
    (Print _ _) -> return (c',tr)
    _c'' -> second (tr ++) <$> evalFocusedWithTrace c'


evalFocused :: Command -> EvalM Command 
evalFocused c = do
  c' <- evalOnce c
  case c' of 
    (Done loc) -> return (Done loc)
    (Err loc err) -> return (Err loc err)
    (Print loc t) -> return (Print loc t)
    _c'' -> evalFocused c'

evalOnce :: Command -> EvalM Command
evalOnce (Err loc err) = return $ Err loc err
evalOnce (Done loc) = return $ Done loc
evalOnce (Print loc t) = return $ Print loc t 
-- substitute variables 
evalOnce (Cut loc t pol (Var loc' v _)) = do 
  u <- lookupBody loc v 
  return $ Cut loc t pol (setLoc loc' u)
-- beta mu
evalOnce (Cut _ t pol (Mu _ v c _)) | isValue pol t = return $ substVar c t v
-- beta shift 
evalOnce (Cut _ (ShiftPos _ t _) CBV (ShiftNeg _ v c _)) = return $ substVar c t v
-- beta K

evalOnce (Cut loc (Xtor _ nm args _) pol (XCase _ pats _)) | all (isValue pol) args = do 
  pt <- findXtor loc nm pats
  substCase loc pt args 
evalOnce cmd = evalOnce (coTrans cmd) 

substCase :: Loc -> Pattern -> [Term] -> EvalM Command
substCase _ MkPattern{ptxt=_, ptv=[], ptcmd=cmd} []  = return cmd
substCase loc MkPattern{ptxt=xt, ptv=(v:vs), ptcmd=cmd} (t:ts) = 
  let newcmd = substVar cmd t v 
  in substCase loc MkPattern{ptxt=xt,ptv=vs,ptcmd=newcmd} ts
substCase loc (MkPattern xt [] _) (_:_) = throwError (ErrXtorArity loc xt) 
substCase loc (MkPattern xt (_:_) _) [] = throwError (ErrXtorArity loc xt) 

findXtor :: Loc -> Xtorname -> [Pattern] -> EvalM Pattern
findXtor loc xt [] = throwError (ErrMissingPt loc xt) 
findXtor loc xt (pt:pts) = if ptxt pt == xt then return pt else findXtor loc xt pts
