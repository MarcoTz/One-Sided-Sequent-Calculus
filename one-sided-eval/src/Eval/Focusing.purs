module Eval.Focusing (
  focus
) where 

import Loc (getLoc)
import Common (EvaluationOrder(..))
import FreeVars.FreeVariables (freshVar)  
import Syntax.Kinded.Terms (Command(..), Term(..), Pattern (..), getType, isValue)

import Prelude ((<$>),(<>), ($))
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe (..))

focus :: Command -> Command 
focus (Done loc)  = Done loc
focus (Err loc err)   = Err loc err
focus (Cut loc t p u) = Cut loc (focusTerm p t) p (focusTerm p u) 
focus (Print loc t) = Print loc t 

focusTerm :: EvaluationOrder -> Term -> Term 
focusTerm _ (Var loc v ty)        = Var loc v ty
focusTerm _ (Mu loc v c ty)       = Mu loc v (focus c) ty
focusTerm pol t@(Xtor loc nm args ty) = do
  let (Tuple vals (Tuple toFocus nonVals)) = splitArgs pol args
  case toFocus of 
    Nothing -> Xtor loc nm (focusTerm pol <$> args) ty
    Just tof -> do 
      let newV = freshVar t
      let newVT = Var (getLoc tof) newV (getType tof)
      let newV2 = freshVar $ Cons t (Cons newVT Nil)
      let tof' = focusTerm pol tof
      let newXtor = focusTerm pol (Xtor loc nm (vals <> Cons newVT Nil <> nonVals) ty)
      let inner = Cut loc newXtor CBV (Var loc newV2 (getType tof))
      let outer = Cut loc tof' CBV (Mu loc newV inner ty)
      Mu loc newV2 outer ty
focusTerm _ (XCase loc pts ty)    = XCase loc (focusPattern <$> pts) ty
focusTerm pol (ShiftCBV loc t ty) = ShiftCBV loc (focusTerm pol t) ty
focusTerm pol (ShiftCBN loc t ty) = ShiftCBN loc (focusTerm pol t) ty 

focusPattern :: Pattern -> Pattern
focusPattern (Pattern pt) = Pattern (pt {ptcmd=focus pt.ptcmd})

splitArgs :: EvaluationOrder -> List Term -> Tuple (List Term) (Tuple (Maybe Term) (List Term))
splitArgs _ Nil = Tuple Nil (Tuple Nothing Nil)
splitArgs pol (Cons a1 as) = do
  let Tuple vals (Tuple toFocus nonVals) = splitArgs pol as
  let nonVals' = getNonVals toFocus nonVals
  if isValue pol a1 then Tuple (Cons a1 vals) (Tuple toFocus nonVals) else (Tuple vals (Tuple (Just a1) nonVals'))
  where 
    getNonVals :: Maybe Term -> List Term -> List Term
    getNonVals Nothing ls = ls
    getNonVals (Just t) ls = Cons t ls
