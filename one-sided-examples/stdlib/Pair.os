module Pair

import Fun 
import Unit

data Pair(a:+,b:+) {
  Tup(a,b)
}

diag :: forall X. X -> Pair(X,X) 
diag := \x. (x,x) 
