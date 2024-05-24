module Pair

import Fun;
import Unit;

data Pair(a:+,b:+) {
  Tup(a,b)
}

diag :: forall X. X -> Pair(X,X);
diag := \x. (x,x);

uncurry :: forall X Y Z. (X -> Y -> Z) -> Pair(X,Y) -> Z;
uncurry := \f. \tp. mu a. <tp | CBV | case { Tup(x,y) => <f x y|CBV|a> }>;
