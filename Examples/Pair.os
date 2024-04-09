module Pair

import Fun; 

data Pair(a:+,b:+) {
  Tup(a,b)
}

diag :: forall X. Fun(X,Pair(X,X));
diag := case { Ap(x,a) => <Tup(x,x) | CBV | a> };
