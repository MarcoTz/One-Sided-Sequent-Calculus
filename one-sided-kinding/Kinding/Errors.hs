module Kinding.Errors where 

import Errors 
import Loc

data KindError where 
  ErrNotImplemented :: Loc -> String -> KindError 
  ErrOther :: Loc -> String -> KindError 

instance Error KindError where 
  getMessage (ErrOther _ msg) = msg 
  getMessage (ErrNotImplemented _ fun) = "Function " <> fun <> " is not yet implemented"

  getLocation (ErrOther loc _) = loc
  getLocation (ErrNotImplemented loc _) = loc

  toError = ErrOther
