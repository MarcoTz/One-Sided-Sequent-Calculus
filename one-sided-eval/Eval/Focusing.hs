module Eval.Focusing (
  focus
) where 

import Syntax.Typed.Terms
import Syntax.Typed.FreeVars
import Common
import Loc

import Data.Set qualified as S

focus :: Command -> Command 
focus (Done loc)  = Done loc
focus (Err loc err)   = Err loc err
focus (Cut loc t p u) = Cut loc (focusTerm p t) p (focusTerm p u) 
focus (Print loc t) = Print loc t 

focusTerm :: Pol -> Term -> Term 
focusTerm _ (Var loc v ty)        = Var loc v ty
focusTerm _ (Mu loc v c ty)       = Mu loc v (focus c) ty
focusTerm pol t@(Xtor loc nm args ty) = do
  let (vals, toFocus, nonVals) = splitArgs pol args
  case toFocus of 
    Nothing -> Xtor loc nm (focusTerm pol <$> args) ty
    Just tof -> do 
      let freeV = freeVars t 
      let newV = freshVar 0 freeV 
      let newV2 = freshVar 0 (S.insert newV freeV)
      let tof' = focusTerm pol tof
      let newXtor = focusTerm pol (Xtor loc nm (vals ++ [Var (getLoc tof) newV (getType tof)] ++ nonVals) ty)
      let inner = Cut loc newXtor Pos (Var loc newV2 (getType tof))
      let outer = Cut loc tof' Pos (Mu loc newV inner ty)
      Mu loc newV2 outer ty
focusTerm _ (XCase loc pts ty)    = XCase loc (focusPattern <$> pts) ty
focusTerm pol (ShiftPos loc t ty)   = ShiftPos loc (focusTerm pol t) ty
focusTerm _ (ShiftNeg loc x c ty) = ShiftNeg loc x (focus c) ty

focusPattern :: Pattern -> Pattern
focusPattern (MkPattern xtn vars c) = MkPattern xtn vars (focus c)

splitArgs :: Pol -> [Term] -> ([Term],Maybe Term, [Term])
splitArgs _ [] = ([],Nothing,[])
splitArgs pol (a1:as) = do
  let (vals,toFocus,nonVals) = splitArgs pol as
  let nonVals' = case toFocus of Nothing -> nonVals; Just tof -> tof:nonVals 
  if isValue pol a1 then (a1:vals,toFocus,nonVals) else (vals,Just a1,nonVals')
