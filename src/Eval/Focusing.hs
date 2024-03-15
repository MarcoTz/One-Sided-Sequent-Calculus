module Eval.Focusing where 

import Syntax.Typed.Terms
import Syntax.Typed.FreeVars
import Common

import Data.Set qualified as S

focus :: Command -> Command 
focus Done        = Done
focus (Err err)   = Err err
focus (Cut t u p) = Cut t u p 

focusTerm :: Pol -> Term -> Term 
focusTerm _ (Var v ty)        = Var v ty
focusTerm _ (Mu v c ty)       = Mu v (focus c) ty
focusTerm pol t@(Xtor nm args ty) = do
  let (vals, toFocus, nonVals) = splitArgs pol args
  case toFocus of 
    Nothing -> Xtor nm (focusTerm pol <$> args) ty
    Just tof -> do 
      let freeV = freeVars t 
      let newV = freshVar 0 freeV 
      let newV2 = freshVar 0 (S.insert newV freeV)
      let tof' = focusTerm pol tof
      let newXtor = focusTerm pol (Xtor nm (vals ++ [Var newV (getType tof)] ++ nonVals) ty)
      let inner = Cut newXtor Pos (Var newV2 (getType tof))
      let outer = Cut tof' Pos (Mu newV inner ty)
      Mu newV2 outer ty
focusTerm _ (XCase pts ty)    = XCase (focusPattern <$> pts) ty
focusTerm pol (ShiftPos t ty)   = ShiftPos (focusTerm pol t) ty
focusTerm _ (ShiftNeg x c ty) = ShiftNeg x (focus c) ty

focusPattern :: Pattern -> Pattern
focusPattern (MkPattern xtn vars c) = MkPattern xtn vars (focus c)

splitArgs :: Pol -> [Term] -> ([Term],Maybe Term, [Term])
splitArgs _ [] = ([],Nothing,[])
splitArgs pol (a1:as) = do
  let (vals,toFocus,nonVals) = splitArgs pol as
  let nonVals' = case toFocus of Nothing -> nonVals; Just tof -> tof:nonVals 
  if isValue pol a1 then (a1:vals,toFocus,nonVals) else (vals,Just a1,nonVals')
