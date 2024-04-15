module Kinding.Errors (
  KindError (..)
)where 

import Loc (Loc)
import Errors (class Error)

import Prelude ((<>))

data KindError =
  ErrNotImplemented Loc String 
  | ErrOther        Loc String 

instance Error KindError where 
  getMessage (ErrOther _ msg) = msg 
  getMessage (ErrNotImplemented _ fun) = "Function " <> fun <> " is not yet implemented"

  getLocation (ErrOther loc _) = loc
  getLocation (ErrNotImplemented loc _) = loc

  toError = ErrOther
