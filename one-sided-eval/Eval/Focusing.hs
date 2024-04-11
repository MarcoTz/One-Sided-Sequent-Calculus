module Eval.Focusing (
  focus
) where 

import Syntax.Kinded.Terms
import Syntax.Kinded.FreeVars
import Common
import Loc

focus :: Command -> Command 
focus (Done loc)  = Done loc
focus (Err loc err)   = Err loc err
focus (Cut loc t p u) = Cut loc (focusTerm p t) p (focusTerm p u) 
focus (Print loc t) = Print loc t 

focusTerm :: EvaluationOrder -> Term -> Term 
focusTerm _ (Var loc v ty)        = Var loc v ty
focusTerm _ (Mu loc v c ty)       = Mu loc v (focus c) ty
focusTerm pol t@(Xtor loc nm args ty) = do
  let (vals, toFocus, nonVals) = splitArgs pol args
  case toFocus of 
    Nothing -> Xtor loc nm (focusTerm pol <$> args) ty
    Just tof -> do 
      let newV = freshVar t
      let newVT = Var (getLoc tof) newV (getType tof)
      let newV2 = freshVar [t,newVT]
      let tof' = focusTerm pol tof
      let newXtor = focusTerm pol (Xtor loc nm (vals ++ [newVT] ++ nonVals) ty)
      let inner = Cut loc newXtor CBV (Var loc newV2 (getType tof))
      let outer = Cut loc tof' CBV (Mu loc newV inner ty)
      Mu loc newV2 outer ty
focusTerm _ (XCase loc pts ty)    = XCase loc (focusPattern <$> pts) ty
focusTerm pol (ShiftCBV loc t ty) = ShiftCBV loc (focusTerm pol t) ty
focusTerm pol (ShiftCBN loc t ty) = ShiftCBN loc (focusTerm pol t) ty 

focusPattern :: Pattern -> Pattern
focusPattern (MkPattern xtn vars c) = MkPattern xtn vars (focus c)

splitArgs :: EvaluationOrder -> [Term] -> ([Term],Maybe Term, [Term])
splitArgs _ [] = ([],Nothing,[])
splitArgs pol (a1:as) = do
  let (vals,toFocus,nonVals) = splitArgs pol as
  let nonVals' = case toFocus of Nothing -> nonVals; Just tof -> tof:nonVals 
  if isValue pol a1 then (a1:vals,toFocus,nonVals) else (vals,Just a1,nonVals')
