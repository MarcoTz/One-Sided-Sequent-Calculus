module Kinding.Errors (
  KindError (..)
)where 

import Loc (Loc)
import Common (Typename,Xtorname,EvaluationOrder, shiftEvalOrder)
import Errors (class Error)

import Prelude ((<>),show)

data KindError =
  ErrTyArity          Loc Typename
  | ErrXtorArity      Loc Xtorname
  | ErrBadPattern Loc
  | ErrShift Loc EvaluationOrder
  | ErrNotImplemented Loc String 
  | ErrOther          Loc String 

instance Error KindError where 
  getMessage (ErrOther _ msg)           = msg 
  getMessage (ErrTyArity _ tyn)         = "Wrong number of type arguments for type " <> show tyn
  getMessage (ErrXtorArity _ xtn)       = "Wrong number of arguments for xtor " <> show xtn
  getMessage (ErrNotImplemented _ fun)  = "Function " <> fun <> " is not yet implemented"
  getMessage (ErrShift _ eo) = "Cannot use kind " <> show eo <> " for shift " <> show (shiftEvalOrder eo)
  getMessage (ErrBadPattern _) = "Bad pattern" 

  getLocation (ErrOther loc _)          = loc
  getLocation (ErrTyArity loc _)        = loc
  getLocation (ErrXtorArity loc _)      = loc
  getLocation (ErrNotImplemented loc _) = loc
  getLocation (ErrShift loc _) = loc
  getLocation (ErrBadPattern loc)       = loc

  toError = ErrOther
